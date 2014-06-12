/* Copyright (C) 2012-2014 Free Software Foundation, Inc.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   GCC is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

/* Virtual Table Pointer Security Pass - Detect corruption of vtable pointers
   before using them for virtual method dispatches.  */

/* This file is part of the vtable security feature implementation.
   The vtable security feature is designed to detect when a virtual
   call is about to be made through an invalid vtable pointer
   (possibly due to data corruption or malicious attacks). The
   compiler finds every virtual call, and inserts a verification call
   before the virtual call.  The verification call takes the actual
   vtable pointer value in the object through which the virtual call
   is being made, and compares the vtable pointer against a set of all
   valid vtable pointers that the object could contain (this set is
   based on the declared type of the object).  If the pointer is in
   the valid set, execution is allowed to continue; otherwise the
   program is halted.

  There are several pieces needed in order to make this work: 1. For
  every virtual class in the program (i.e. a class that contains
  virtual methods), we need to build the set of all possible valid
  vtables that an object of that class could point to.  This includes
  vtables for any class(es) that inherit from the class under
  consideration.  2. For every such data set we build up, we need a
  way to find and reference the data set.  This is complicated by the
  fact that the real vtable addresses are not known until runtime,
  when the program is loaded into memory, but we need to reference the
  sets at compile time when we are inserting verification calls into
  the program.  3.  We need to find every virtual call in the program,
  and insert the verification call (with the appropriate arguments)
  before the virtual call.  4. We need some runtime library pieces:
  the code to build up the data sets at runtime; the code to actually
  perform the verification using the data sets; and some code to set
  protections on the data sets, so they themselves do not become
  hacker targets.

  To find and reference the set of valid vtable pointers for any given
  virtual class, we create a special global varible for each virtual
  class.  We refer to this as the "vtable map variable" for that
  class.  The vtable map variable has the type "void *", and is
  initialized by the compiler to NULL.  At runtime when the set of
  valid vtable pointers for a virtual class, e.g. class Foo, is built,
  the vtable map variable for class Foo is made to point to the set.
  During compile time, when the compiler is inserting verification
  calls into the program, it passes the vtable map variable for the
  appropriate class to the verification call, so that at runtime the
  verification call can find the appropriate data set.

  The actual set of valid vtable pointers for a virtual class,
  e.g. class Foo, cannot be built until runtime, when the vtables get
  loaded into memory and their addresses are known.  But the knowledge
  about which vtables belong in which class' hierarchy is only known
  at compile time.  Therefore at compile time we collect class
  hierarchy and vtable information about every virtual class, and we
  generate calls to build up the data sets at runtime.  To build the
  data sets, we call one of the functions we add to the runtime
  library, __VLTRegisterPair.  __VLTRegisterPair takes two arguments,
  a vtable map variable and the address of a vtable.  If the vtable
  map variable is currently NULL, it creates a new data set (hash
  table), makes the vtable map variable point to the new data set, and
  inserts the vtable address into the data set.  If the vtable map
  variable is not NULL, it just inserts the vtable address into the
  data set.  In order to make sure that our data sets are built before
  any verification calls happen, we create a special constructor
  initialization function for each compilation unit, give it a very
  high initialization priority, and insert all of our calls to
  __VLTRegisterPair into our special constructor initialization
  function.

  The vtable verification feature is controlled by the flag
  '-fvtable-verify='.  There are three flavors of this:
  '-fvtable-verify=std', '-fvtable-verify=preinit', and
  '-fvtable-verify=none'.  If the option '-fvtable-verfy=preinit' is
  used, then our constructor initialization function gets put into the
  preinit array.  This is necessary if there are data sets that need
  to be built very early in execution.  If the constructor
  initialization function gets put into the preinit array, the we also
  add calls to __VLTChangePermission at the beginning and end of the
  function.  The call at the beginning sets the permissions on the
  data sets and vtable map variables to read/write, and the one at the
  end makes them read-only.  If the '-fvtable-verify=std' option is
  used, the constructor initialization functions are executed at their
  normal time, and the __VLTChangePermission calls are handled
  differently (see the comments in libstdc++-v3/libsupc++/vtv_rts.cc).
  The option '-fvtable-verify=none' turns off vtable verification.

  This file contains code to find and record the class hierarchies for
  the virtual classes in a program, and all the vtables associated
  with each such class; to generate the vtable map variables; and to
  generate the constructor initialization function (with the calls to
  __VLTRegisterPair, and __VLTChangePermission).  The main data
  structures used for collecting the class hierarchy data and
  building/maintaining the vtable map variable data are defined in
  gcc/vtable-verify.h, because they are used both here and in
  gcc/vtable-verify.c.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "cp-tree.h"
#include "output.h"
#include "cgraph.h"
#include "tree-iterator.h"
#include "vtable-verify.h"
#include "gimplify.h"
#include "stringpool.h"
#include "stor-layout.h"

static int num_calls_to_regset = 0;
static int num_calls_to_regpair = 0;
static int current_set_size;

/* Mark these specially since they need to be stored in precompiled
   header IR.  */
static GTY (()) vec<tree, va_gc> *vlt_saved_class_info;
static GTY (()) tree vlt_register_pairs_fndecl = NULL_TREE;
static GTY (()) tree vlt_register_set_fndecl = NULL_TREE;

struct work_node {
  struct vtv_graph_node *node;
  struct work_node *next;
};

struct vtbl_map_node *vtable_find_or_create_map_decl (tree);

/* As part of vtable verification the compiler generates and inserts
   calls to __VLTVerifyVtablePointer, which is in libstdc++.  This
   function builds and initializes the function decl that is used
   in generating those function calls.

   In addition to __VLTVerifyVtablePointer there is also
   __VLTVerifyVtablePointerDebug which can be used in place of
   __VLTVerifyVtablePointer, and which takes extra parameters and
   outputs extra information, to help debug problems.  The debug
   version of this function is generated and used if flag_vtv_debug is
   true.

   The signatures for these functions are:

   void * __VLTVerifyVtablePointer (void **, void*);
   void * __VLTVerifyVtablePointerDebug (void**, void *, char *, char *);
*/

void
vtv_build_vtable_verify_fndecl (void)
{
  tree func_type = NULL_TREE;

  if (verify_vtbl_ptr_fndecl != NULL_TREE
      && TREE_CODE (verify_vtbl_ptr_fndecl) != ERROR_MARK)
    return;

  if (flag_vtv_debug)
    {
      func_type = build_function_type_list (const_ptr_type_node,
                                            build_pointer_type (ptr_type_node),
                                            const_ptr_type_node,
                                            const_string_type_node,
                                            const_string_type_node,
                                            NULL_TREE);
      verify_vtbl_ptr_fndecl =
        build_lang_decl (FUNCTION_DECL,
                         get_identifier ("__VLTVerifyVtablePointerDebug"),
                         func_type);
    }
  else
    {
      func_type = build_function_type_list (const_ptr_type_node,
                                            build_pointer_type (ptr_type_node),
                                            const_ptr_type_node,
                                            NULL_TREE);
      verify_vtbl_ptr_fndecl =
        build_lang_decl (FUNCTION_DECL,
                         get_identifier ("__VLTVerifyVtablePointer"),
                         func_type);
    }

  TREE_NOTHROW (verify_vtbl_ptr_fndecl) = 1;
  DECL_ATTRIBUTES (verify_vtbl_ptr_fndecl)
      = tree_cons (get_identifier ("leaf"), NULL,
                   DECL_ATTRIBUTES (verify_vtbl_ptr_fndecl));
  DECL_PURE_P (verify_vtbl_ptr_fndecl) = 1;
  TREE_PUBLIC (verify_vtbl_ptr_fndecl) = 1;
  DECL_PRESERVE_P (verify_vtbl_ptr_fndecl) = 1;
}

/* As part of vtable verification the compiler generates and inserts
   calls to __VLTRegisterSet and __VLTRegisterPair, which are in
   libsupc++.  This function builds and initializes the function decls
   that are used in generating those function calls.

   The signatures for these functions are:

   void __VLTRegisterSetDebug (void **, const void *, std::size_t,
                               size_t, void **);

   void __VLTRegisterSet (void **, const void *, std::size_t,
                          size_t, void **);

   void __VLTRegisterPairDebug (void **, const void *, size_t,
                                const void *, const char *, const char *);

   void __VLTRegisterPair (void **, const void *, size_t, const void *);
*/

static void
init_functions (void)
{
  tree register_set_type;
  tree register_pairs_type;

  if (vlt_register_set_fndecl != NULL_TREE)
    return;

  gcc_assert (vlt_register_pairs_fndecl == NULL_TREE);
  gcc_assert (vlt_register_set_fndecl == NULL_TREE);

  /* Build function decl for __VLTRegisterSet*.  */

  register_set_type = build_function_type_list
                                             (void_type_node,
                                              build_pointer_type (ptr_type_node),
                                              const_ptr_type_node,
                                              size_type_node,
                                              size_type_node,
                                              build_pointer_type (ptr_type_node),
                                              NULL_TREE);

  if (flag_vtv_debug)
    vlt_register_set_fndecl = build_lang_decl
                                       (FUNCTION_DECL,
                                        get_identifier ("__VLTRegisterSetDebug"),
                                        register_set_type);
  else
    vlt_register_set_fndecl = build_lang_decl
                                       (FUNCTION_DECL,
                                        get_identifier ("__VLTRegisterSet"),
                                        register_set_type);


  TREE_NOTHROW (vlt_register_set_fndecl) = 1;
  DECL_ATTRIBUTES (vlt_register_set_fndecl) =
                    tree_cons (get_identifier ("leaf"), NULL,
                               DECL_ATTRIBUTES (vlt_register_set_fndecl));
  DECL_EXTERNAL(vlt_register_set_fndecl) = 1;
  TREE_PUBLIC (vlt_register_set_fndecl) = 1;
  DECL_PRESERVE_P (vlt_register_set_fndecl) = 1;
  SET_DECL_LANGUAGE (vlt_register_set_fndecl, lang_cplusplus);

  /* Build function decl for __VLTRegisterPair*.  */

  if (flag_vtv_debug)
    {
      register_pairs_type = build_function_type_list (void_type_node,
                                                      build_pointer_type
                                                              (ptr_type_node),
                                                      const_ptr_type_node,
                                                      size_type_node,
                                                      const_ptr_type_node,
                                                      const_string_type_node,
                                                      const_string_type_node,
                                                      NULL_TREE);

      vlt_register_pairs_fndecl = build_lang_decl
                                      (FUNCTION_DECL,
                                       get_identifier ("__VLTRegisterPairDebug"),
                                       register_pairs_type);
    }
  else
    {
      register_pairs_type = build_function_type_list (void_type_node,
                                                      build_pointer_type
                                                              (ptr_type_node),
                                                      const_ptr_type_node,
                                                      size_type_node,
                                                      const_ptr_type_node,
                                                      NULL_TREE);

      vlt_register_pairs_fndecl = build_lang_decl
                                      (FUNCTION_DECL,
                                       get_identifier ("__VLTRegisterPair"),
                                       register_pairs_type);
    }

  TREE_NOTHROW (vlt_register_pairs_fndecl) = 1;
  DECL_ATTRIBUTES (vlt_register_pairs_fndecl) =
                    tree_cons (get_identifier ("leaf"), NULL,
                               DECL_ATTRIBUTES (vlt_register_pairs_fndecl));
  DECL_EXTERNAL(vlt_register_pairs_fndecl) = 1;
  TREE_PUBLIC (vlt_register_pairs_fndecl) = 1;
  DECL_PRESERVE_P (vlt_register_pairs_fndecl) = 1;
  SET_DECL_LANGUAGE (vlt_register_pairs_fndecl, lang_cplusplus);

}

/* This is a helper function for
   vtv_compute_class_hierarchy_transitive_closure.  It adds a
   vtv_graph_node to the WORKLIST, which is a linked list of
   seen-but-not-yet-processed nodes.  INSERTED is a bitmap, one bit
   per node, to help make sure that we don't insert a node into the
   worklist more than once.  Each node represents a class somewhere in
   our class hierarchy information. Every node in the graph gets added
   to the worklist exactly once and removed from the worklist exactly
   once (when all of its children have been processed).  */

static void
add_to_worklist (struct work_node **worklist, struct vtv_graph_node *node,
                 sbitmap inserted)
{
  struct work_node *new_work_node;

  if (bitmap_bit_p (inserted, node->class_uid))
    return;

  new_work_node = XNEW (struct work_node);
  new_work_node->next = *worklist;
  new_work_node->node = node;
  *worklist = new_work_node;

  bitmap_set_bit (inserted, node->class_uid);
}

/* This is a helper function for
   vtv_compute_class_hierarchy_transitive_closure.  It goes through
   the WORKLIST of class hierarchy nodes looking for a "leaf" node,
   i.e. a node whose children in the hierarchy have all been
   processed.  When it finds the next leaf node, it removes it from
   the linked list (WORKLIST) and returns the node.  */

static struct vtv_graph_node *
find_and_remove_next_leaf_node (struct work_node **worklist)
{
  struct work_node *prev, *cur;
  struct vtv_graph_node *ret_val = NULL;

  for (prev = NULL, cur = *worklist; cur; prev = cur, cur = cur->next)
    {
      if ((cur->node->children).length() == cur->node->num_processed_children)
        {
          if (prev == NULL)
            (*worklist) = cur->next;
          else
            prev->next = cur->next;

          cur->next = NULL;
          ret_val = cur->node;
          free (cur);
          return ret_val;
        }
    }

  return NULL;
}

/* In our class hierarchy graph, each class node contains a bitmap,
   with one bit for each class in the hierarchy.  The bits are set for
   classes that are descendants in the graph of the current node.
   Initially the descendants bitmap is only set for immediate
   descendants.  This function traverses the class hierarchy graph,
   bottom up, filling in the transitive closures for the descendants
   as we rise up the graph.  */

void
vtv_compute_class_hierarchy_transitive_closure (void)
{
  struct work_node *worklist = NULL;
  sbitmap inserted = sbitmap_alloc (num_vtable_map_nodes);
  unsigned i;
  unsigned j;

  /* Note: Every node in the graph gets added to the worklist exactly
   once and removed from the worklist exactly once (when all of its
   children have been processed).  Each node's children edges are
   followed exactly once, and each node's parent edges are followed
   exactly once.  So this algorithm is roughly O(V + 2E), i.e.
   O(E + V).  */

  /* Set-up:                                                                */
  /* Find all the "leaf" nodes in the graph, and add them to the worklist.  */
  bitmap_clear (inserted);
  for (j = 0; j < num_vtable_map_nodes; ++j)
    {
      struct vtbl_map_node *cur = vtbl_map_nodes_vec[j];
      if (cur->class_info
          && ((cur->class_info->children).length() == 0)
          && ! (bitmap_bit_p (inserted, cur->class_info->class_uid)))
        add_to_worklist (&worklist, cur->class_info, inserted);
    }

  /* Main work: pull next leaf node off work list, process it, add its
     parents to the worklist, where a 'leaf' node is one that has no
     children, or all of its children have been processed.  */
  while (worklist)
    {
      struct vtv_graph_node *temp_node =
                                  find_and_remove_next_leaf_node (&worklist);

      gcc_assert (temp_node != NULL);
      temp_node->descendants = sbitmap_alloc (num_vtable_map_nodes);
      bitmap_clear (temp_node->descendants);
      bitmap_set_bit (temp_node->descendants, temp_node->class_uid);
      for (i = 0; i < (temp_node->children).length(); ++i)
        bitmap_ior (temp_node->descendants, temp_node->descendants,
                        temp_node->children[i]->descendants);
      for (i = 0; i < (temp_node->parents).length(); ++i)
        {
          temp_node->parents[i]->num_processed_children =
                    temp_node->parents[i]->num_processed_children + 1;
          if (!bitmap_bit_p (inserted, temp_node->parents[i]->class_uid))
            add_to_worklist (&worklist, temp_node->parents[i], inserted);
        }
    }
}

/* Keep track of which pairs we have already created __VLTRegisterPair
   calls for, to prevent creating duplicate calls within the same
   compilation unit.  VTABLE_DECL is the var decl for the vtable of
   the (descendant) class that we are adding to our class hierarchy
   data.  VPTR_ADDRESS is an expression for calculating the correct
   offset into the vtable (VTABLE_DECL).  It is the actual vtable
   pointer address that will be stored in our list of valid vtable
   pointers for BASE_CLASS.  BASE_CLASS is the record_type node for
   the base class to whose hiearchy we want to add
   VPTR_ADDRESS. (VTABLE_DECL should be the vtable for BASE_CLASS or
   one of BASE_CLASS' descendents.  */

static bool
check_and_record_registered_pairs (tree vtable_decl, tree vptr_address,
                                   tree base_class)
{
  unsigned offset;
  struct vtbl_map_node *base_vtable_map_node;
  bool inserted_something = false;


  if (TREE_CODE (vptr_address) == ADDR_EXPR
      && TREE_CODE (TREE_OPERAND (vptr_address, 0)) == MEM_REF)
    vptr_address = TREE_OPERAND (vptr_address, 0);

  if (TREE_OPERAND_LENGTH (vptr_address) > 1)
    offset = TREE_INT_CST_LOW (TREE_OPERAND (vptr_address, 1));
  else
    offset = 0;

  base_vtable_map_node = vtbl_map_get_node (TYPE_MAIN_VARIANT (base_class));

  inserted_something = vtbl_map_node_registration_insert
                                                        (base_vtable_map_node,
                                                         vtable_decl,
                                                         offset);
  return !inserted_something;
}

/* Given an IDENTIFIER_NODE, build and return a string literal based on it.  */

static tree
build_string_from_id (tree identifier)
{
  int len;

  gcc_assert (TREE_CODE (identifier) == IDENTIFIER_NODE);

  len = IDENTIFIER_LENGTH (identifier);
  return build_string_literal (len + 1, IDENTIFIER_POINTER (identifier));
}

/* A class may contain secondary vtables in it, for various reasons.
   This function goes through the decl chain of a class record looking
   for any fields that point to secondary vtables, and adding calls to
   __VLTRegisterPair for the secondary vtable pointers.

   BASE_CLASS_DECL_ARG is an expression for the address of the vtable
   map variable for the BASE_CLASS (whose hierarchy we are currently
   updating).  BASE_CLASS is the record_type node for the base class.
   RECORD_TYPE is the record_type node for the descendant class that
   we are possibly adding to BASE_CLASS's hierarchy.  BODY is the
   function body for the constructor init function to which we are
   adding our calls to __VLTRegisterPair.  */

static void
register_construction_vtables (tree base_class, tree record_type,
                               vec<tree> *vtable_ptr_array)
{
  tree vtbl_var_decl;

  if (TREE_CODE (record_type) != RECORD_TYPE)
    return;

  vtbl_var_decl = CLASSTYPE_VTABLES (record_type);

  if (CLASSTYPE_VBASECLASSES (record_type))
    {
      tree vtt_decl;
      bool already_registered = false;
      tree val_vtbl_decl = NULL_TREE;

      vtt_decl = DECL_CHAIN (vtbl_var_decl);

      /* Check to see if we have found a VTT.  Add its data if appropriate.  */
      if (vtt_decl)
        {
          tree values = DECL_INITIAL (vtt_decl);
          if (TREE_ASM_WRITTEN (vtt_decl)
              && values != NULL_TREE
              && TREE_CODE (values) == CONSTRUCTOR
              && TREE_CODE (TREE_TYPE (values)) == ARRAY_TYPE)
            {
              unsigned HOST_WIDE_INT cnt;
              constructor_elt *ce;

              /* Loop through the initialization values for this
                 vtable to get all the correct vtable pointer
                 addresses that we need to add to our set of valid
                 vtable pointers for the current base class.  This may
                 result in adding more than just the element assigned
                 to the primary vptr of the class, so we may end up
                 with more vtable pointers than are strictly
                 necessary.  */

              for (cnt = 0;
                   vec_safe_iterate (CONSTRUCTOR_ELTS (values),
                                     cnt, &ce);
                   cnt++)
                {
                  tree value = ce->value;

                  /* Search for the ADDR_EXPR operand within the value.  */

                  while (value
                         && TREE_OPERAND (value, 0)
                         && TREE_CODE (TREE_OPERAND (value, 0)) == ADDR_EXPR)
                    value = TREE_OPERAND (value, 0);

                  /* The VAR_DECL for the vtable should be the first
                     argument of the ADDR_EXPR, which is the first
                     argument of value.*/

                  if (TREE_OPERAND (value, 0))
                    val_vtbl_decl = TREE_OPERAND (value, 0);

                  while (TREE_CODE (val_vtbl_decl) != VAR_DECL
                         && TREE_OPERAND (val_vtbl_decl, 0))
                    val_vtbl_decl = TREE_OPERAND (val_vtbl_decl, 0);

                  gcc_assert (TREE_CODE (val_vtbl_decl) == VAR_DECL);

                  /* Check to see if we already have this vtable pointer in
                     our valid set for this base class.  */

                  already_registered = check_and_record_registered_pairs
                                                               (val_vtbl_decl,
                                                                value,
                                                                base_class);

                  if (already_registered)
                    continue;

                  /* Add this vtable pointer to our set of valid
                     pointers for the base class.  */

                  vtable_ptr_array->safe_push (value);
                  current_set_size++;
                }
            }
        }
    }
}

/* This function iterates through all the vtables it can find from the
   BINFO of a class, to make sure we have found ALL of the vtables
   that an object of that class could point to.  Generate calls to
   __VLTRegisterPair for those vtable pointers that we find.

   BINFO is the tree_binfo node for the BASE_CLASS.  BODY is the
   function body for the constructor init function to which we are
   adding calls to __VLTRegisterPair.  ARG1 is an expression for the
   address of the vtable map variable (for the BASE_CLASS), that will
   point to the updated data set.  BASE_CLASS is the record_type node
   for the base class whose set of valid vtable pointers we are
   updating. STR1 and STR2 are all debugging information, to be passed
   as parameters to __VLTRegisterPairDebug.  STR1 represents the name
   of the vtable map variable to be updated by the call.  Similarly,
   STR2 represents the name of the class whose vtable pointer is being
   added to the hierarchy.  */

static void
register_other_binfo_vtables (tree binfo, tree base_class,
                              vec<tree> *vtable_ptr_array)
{
  unsigned ix;
  tree base_binfo;
  tree vtable_decl;
  bool already_registered;

  if (binfo == NULL_TREE)
    return;

  for (ix = 0; BINFO_BASE_ITERATE (binfo, ix, base_binfo); ix++)
    {
      if ((!BINFO_PRIMARY_P (base_binfo)
           || BINFO_VIRTUAL_P (base_binfo))
          && (vtable_decl = get_vtbl_decl_for_binfo (base_binfo)))
        {
          tree vtable_address = build_vtbl_address (base_binfo);

          already_registered = check_and_record_registered_pairs
                                                              (vtable_decl,
                                                               vtable_address,
                                                               base_class);
          if (!already_registered)
            {
              vtable_ptr_array->safe_push (vtable_address);
              current_set_size++;
            }
        }

      register_other_binfo_vtables (base_binfo, base_class, vtable_ptr_array);
    }
}

/* The set of valid vtable pointers for any given class are stored in
   a hash table.  For reasons of efficiency, that hash table size is
   always a power of two.  In order to try to prevent re-sizing the
   hash tables very often, we pass __VLTRegisterPair an initial guess
   as to the number of entries the hashtable will eventually need
   (rounded up to the nearest power of two).  This function takes the
   class information we have collected for a particular class,
   CLASS_NODE, and calculates the hash table size guess.  */

static int
guess_num_vtable_pointers (struct vtv_graph_node *class_node)
{
  tree vtbl;
  int total_num_vtbls = 0;
  int num_vtbls_power_of_two = 1;
  unsigned i;

  for (i = 0; i < num_vtable_map_nodes; ++i)
    if (bitmap_bit_p (class_node->descendants, i))
      {
        tree class_type = vtbl_map_nodes_vec[i]->class_info->class_type;
        for (vtbl = CLASSTYPE_VTABLES (class_type); vtbl;
             vtbl = DECL_CHAIN (vtbl))
          {
            total_num_vtbls++;
            if (total_num_vtbls > num_vtbls_power_of_two)
              num_vtbls_power_of_two <<= 1;
          }
      }
  return num_vtbls_power_of_two;
}

/* A simple hash function on strings */
/* Be careful about changing this routine. The values generated will
   be stored in the calls to InitSet. So, changing this routine may
   cause a binary incompatibility.  */

static uint32_t
vtv_string_hash (const char *in)
{
  const char *s = in;
  uint32_t h = 0;

  gcc_assert (in != NULL);
  for ( ; *s; ++s)
    h = 5 * h + *s;
  return h;
}

static char *
get_log_file_name (const char *fname)
{
  const char *tmp_dir = concat (dump_dir_name, NULL);
  char *full_name;
  int dir_len;
  int fname_len;

  dir_len = strlen (tmp_dir);
  fname_len = strlen (fname);

  full_name = XNEWVEC (char, dir_len + fname_len + 1);
  strcpy (full_name, tmp_dir);
  strcpy (full_name + dir_len, fname);

  return full_name;
}

static void
write_out_current_set_data (tree base_class, int set_size)
{
  static int class_data_log_fd = -1;
  char buffer[1024];
  int bytes_written __attribute__ ((unused));
  char *file_name = get_log_file_name ("vtv_class_set_sizes.log");

  if (class_data_log_fd == -1)
    class_data_log_fd = open (file_name,
                              O_WRONLY | O_APPEND | O_CREAT, S_IRWXU);

  if (class_data_log_fd == -1)
    {
      warning_at (UNKNOWN_LOCATION, 0,
		  "unable to open log file %<vtv_class_set_sizes.log%>: %m");
      return;
    }

  snprintf (buffer, sizeof (buffer), "%s %d\n",
            IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (TYPE_NAME (base_class))),
            set_size);
  bytes_written = write (class_data_log_fd, buffer, strlen (buffer));
}

static tree
build_key_buffer_arg (tree base_ptr_var_decl)
{
  const int key_type_fixed_size = 8;
  uint32_t len1 = IDENTIFIER_LENGTH (DECL_NAME (base_ptr_var_decl));
  uint32_t hash_value = vtv_string_hash (IDENTIFIER_POINTER
                                              (DECL_NAME (base_ptr_var_decl)));
  void *key_buffer = xmalloc (len1 + key_type_fixed_size);
  uint32_t *value_ptr = (uint32_t *) key_buffer;
  tree ret_value;

  /* Set the len and hash for the string.  */
  *value_ptr = len1;
  value_ptr++;
  *value_ptr = hash_value;

  /* Now copy the string representation of the vtbl map name...  */
  memcpy ((char *) key_buffer + key_type_fixed_size,
          IDENTIFIER_POINTER (DECL_NAME (base_ptr_var_decl)),
          len1);

  /* ... and build a string literal from it. This will make a copy
     so the key_bufffer is not needed anymore after this.  */
  ret_value = build_string_literal (len1 + key_type_fixed_size,
                                    (char *) key_buffer);
  free (key_buffer);
  return ret_value;
}

static void
insert_call_to_register_set (tree class_name,
                             vec<tree> *vtbl_ptr_array, tree body, tree arg1,
                             tree arg2, tree size_hint_arg)
{
  tree call_expr;
  int num_args = vtbl_ptr_array->length();
  char *array_arg_name = ACONCAT (("__vptr_array_",
                                   IDENTIFIER_POINTER (class_name), NULL));
  tree array_arg_type = build_array_type_nelts (build_pointer_type
                                                  (build_pointer_type
                                                     (void_type_node)),
                                                num_args);
  tree array_arg = build_decl (UNKNOWN_LOCATION, VAR_DECL,
                               get_identifier (array_arg_name),
                               array_arg_type);
  int k;

  vec<constructor_elt, va_gc> *array_elements;
  vec_alloc (array_elements, num_args);
                                                        
  tree initial = NULL_TREE;
  tree arg3 = NULL_TREE;

  TREE_PUBLIC (array_arg) = 0;
  DECL_EXTERNAL (array_arg) = 0;
  TREE_STATIC (array_arg) = 1;
  DECL_ARTIFICIAL (array_arg) = 0;
  TREE_READONLY (array_arg) = 1;
  DECL_IGNORED_P (array_arg) = 0;
  DECL_PRESERVE_P (array_arg) = 0;
  DECL_VISIBILITY (array_arg) = VISIBILITY_HIDDEN;

  for (k = 0; k < num_args; ++k)
    {
      CONSTRUCTOR_APPEND_ELT (array_elements, NULL_TREE, (*vtbl_ptr_array)[k]);
    }

  initial = build_constructor (TREE_TYPE (array_arg), array_elements);

  TREE_CONSTANT (initial) = 1;
  TREE_STATIC (initial) = 1;
  DECL_INITIAL (array_arg) = initial;
  relayout_decl (array_arg);
  varpool_finalize_decl (array_arg);

  arg3 = build1 (ADDR_EXPR, TYPE_POINTER_TO (TREE_TYPE (array_arg)), array_arg);

  TREE_TYPE (arg3) = build_pointer_type (TREE_TYPE (array_arg));

  call_expr = build_call_expr (vlt_register_set_fndecl, 5, arg1,
                               arg2, /* set_symbol_key */
                               size_hint_arg, build_int_cst (size_type_node,
                                                             num_args),
                               arg3);
  append_to_statement_list (call_expr, &body);
  num_calls_to_regset++;
}

static void
insert_call_to_register_pair (vec<tree> *vtbl_ptr_array, tree arg1,
                              tree arg2, tree size_hint_arg, tree str1,
                              tree str2, tree body)
{
  tree call_expr;
  int num_args = vtbl_ptr_array->length();
  tree vtable_address = NULL_TREE;

  if (num_args == 0)
    vtable_address = build_int_cst (build_pointer_type (void_type_node), 0);
  else
    vtable_address = (*vtbl_ptr_array)[0];

  if (flag_vtv_debug)
    call_expr = build_call_expr (vlt_register_pairs_fndecl, 6, arg1, arg2,
                                 size_hint_arg, vtable_address, str1, str2);
  else
    call_expr = build_call_expr (vlt_register_pairs_fndecl, 4, arg1, arg2,
                                 size_hint_arg, vtable_address);
    
  append_to_statement_list (call_expr, &body);
  num_calls_to_regpair++;
}

static void
output_set_info (tree record_type, vec<tree> vtbl_ptr_array)
{
  static int vtv_debug_log_fd = -1;
  char buffer[1024];
  int bytes_written __attribute__ ((unused));
  int array_len = vtbl_ptr_array.length();
  const char *class_name =
              IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (TYPE_NAME (record_type)));
  char *file_name = get_log_file_name ("vtv_set_ptr_data.log");

  if (vtv_debug_log_fd == -1)
    vtv_debug_log_fd = open (file_name,
                             O_WRONLY | O_APPEND | O_CREAT, S_IRWXU);
  if (vtv_debug_log_fd == -1)
    {
      warning_at (UNKNOWN_LOCATION, 0,
		  "unable to open log file %<vtv_set_ptr_data.log%>: %m");
      return;
    }

  for (int i = 0; i < array_len; ++i)
    {
      const char *vptr_name = "unknown";
      int vptr_offset = 0;
      
      if (TREE_CODE (vtbl_ptr_array[i]) == POINTER_PLUS_EXPR)
        {
          tree arg0 = TREE_OPERAND (vtbl_ptr_array[i], 0);
          tree arg1 = TREE_OPERAND (vtbl_ptr_array[i], 1);

          if (TREE_CODE (arg0) == ADDR_EXPR)
            arg0 = TREE_OPERAND (arg0, 0);

          if (TREE_CODE (arg0) == VAR_DECL)
            vptr_name = IDENTIFIER_POINTER (DECL_NAME (arg0));

          if (TREE_CODE (arg1) == INTEGER_CST)
            vptr_offset = TREE_INT_CST_LOW (arg1);
        }

      snprintf (buffer, sizeof (buffer), "%s %s %s + %d\n",
                main_input_filename, class_name, vptr_name, vptr_offset);
      bytes_written = write (vtv_debug_log_fd, buffer, strlen(buffer));
    }

}

/* This function goes through our internal class hierarchy & vtable
   pointer data structure and outputs calls to __VLTRegisterPair for
   every class-vptr pair (for those classes whose vtable would be
   output in the current compilation unit).  These calls get put into
   our constructor initialization function.  BODY is the function
   body, so far, of our constructor initialization function, to which we
   add the calls.  */

static bool
register_all_pairs (tree body)
{
  bool registered_at_least_one = false;
  vec<tree> *vtbl_ptr_array = NULL;
  unsigned j;

  for (j = 0; j < num_vtable_map_nodes; ++j)
    {
      struct vtbl_map_node *current = vtbl_map_nodes_vec[j];
      unsigned i = 0;
      tree base_class = current->class_info->class_type;
      tree base_ptr_var_decl = current->vtbl_map_decl;
      tree arg1;
      tree arg2;
      tree new_type;
      tree str1 = NULL_TREE;
      tree str2 = NULL_TREE;
      size_t size_hint;
      tree size_hint_arg;

      gcc_assert (current->class_info != NULL);


      if (flag_vtv_debug)
        str1 = build_string_from_id (DECL_NAME (base_ptr_var_decl));

      new_type = build_pointer_type (TREE_TYPE (base_ptr_var_decl));
      arg1 = build1 (ADDR_EXPR, new_type, base_ptr_var_decl);

      /* We need a fresh vector for each iteration.  */
      if (vtbl_ptr_array)
	vec_free (vtbl_ptr_array);

      vec_alloc (vtbl_ptr_array, 10);

      for (i = 0; i < num_vtable_map_nodes; ++i)
        if (bitmap_bit_p (current->class_info->descendants, i))
          {
            struct vtbl_map_node *vtbl_class_node = vtbl_map_nodes_vec[i];
            tree class_type = vtbl_class_node->class_info->class_type;

            if (class_type
                && (TREE_CODE (class_type) == RECORD_TYPE))
              {
                bool already_registered;

                tree binfo = TYPE_BINFO (class_type);
                tree vtable_decl;
                bool vtable_should_be_output = false;

                vtable_decl = CLASSTYPE_VTABLES (class_type);

                /* Handle main vtable for this class.  */

                if (vtable_decl)
                  {
                    vtable_should_be_output = TREE_ASM_WRITTEN (vtable_decl);
                    str2 = build_string_from_id (DECL_NAME (vtable_decl));
                  }

                if (vtable_decl && vtable_should_be_output)
                  {
                    tree vtable_address = build_vtbl_address (binfo);

                    already_registered = check_and_record_registered_pairs
                                                              (vtable_decl,
                                                               vtable_address,
                                                               base_class);


                    if (!already_registered)
                      {
                        vtbl_ptr_array->safe_push (vtable_address);

                        /* Find and handle any 'extra' vtables associated
                           with this class, via virtual inheritance.   */
                        register_construction_vtables (base_class, class_type,
                                                       vtbl_ptr_array);

                        /* Find and handle any 'extra' vtables associated
                           with this class, via multiple inheritance.   */
                        register_other_binfo_vtables (binfo, base_class,
                                                      vtbl_ptr_array);
                      }
                  }
              }
          }
      current_set_size = vtbl_ptr_array->length();

      /* Sometimes we need to initialize the set symbol even if we are
         not adding any vtable pointers to the set in the current
         compilation unit.  In that case, we need to initialize the
         set to our best guess as to what the eventual size of the set
         hash table will be (to prevent having to re-size the hash
         table later).  */

      size_hint = guess_num_vtable_pointers (current->class_info);

      /* If we have added vtable pointers to the set in this
         compilation unit, adjust the size hint for the set's hash
         table appropriately.  */
      if (vtbl_ptr_array->length() > 0)
	{
	  unsigned len = vtbl_ptr_array->length();
	  while ((size_t) len > size_hint)
	    size_hint <<= 1;
	}
      size_hint_arg = build_int_cst (size_type_node, size_hint);

      /* Get the key-buffer argument.  */
      arg2 = build_key_buffer_arg (base_ptr_var_decl);

      if (str2 == NULL_TREE)
        str2 = build_string_literal (strlen ("unknown") + 1,
                                     "unknown");

      if (flag_vtv_debug)
        output_set_info (current->class_info->class_type,
                         *vtbl_ptr_array);

      if (vtbl_ptr_array->length() > 1)
        {
          insert_call_to_register_set (current->class_name,
                                       vtbl_ptr_array, body, arg1, arg2,
                                       size_hint_arg);
          registered_at_least_one = true;
        }
      else
        {

          if (vtbl_ptr_array->length() > 0
              || (current->is_used
                  || (current->registered.size() > 0)))
            {
              insert_call_to_register_pair (vtbl_ptr_array,
                                            arg1, arg2, size_hint_arg, str1,
                                            str2, body);
              registered_at_least_one = true;
            }
        }

      if (flag_vtv_counts && current_set_size > 0)
        write_out_current_set_data (base_class, current_set_size);

    }

  return registered_at_least_one;
}

/* Given a tree containing a class type (CLASS_TYPE), this function
   finds and returns the class hierarchy node for that class in our
   data structure.  */

static struct vtv_graph_node *
find_graph_node (tree class_type)
{
  struct vtbl_map_node *vtbl_node;

  vtbl_node = vtbl_map_get_node (TYPE_MAIN_VARIANT (class_type));
  if (vtbl_node)
    return vtbl_node->class_info;

  return NULL;
}

/* Add base class/derived class pair to our internal class hierarchy
   data structure.  BASE_NODE is our vtv_graph_node that corresponds
   to a base class.  DERIVED_NODE is our vtv_graph_node that
   corresponds to a class that is a descendant of the base class
   (possibly the base class itself).  */

static void
add_hierarchy_pair (struct vtv_graph_node *base_node,
                    struct vtv_graph_node *derived_node)
{
  (base_node->children).safe_push (derived_node);
  (derived_node->parents).safe_push (base_node);
}

/* This functions adds a new base class/derived class relationship to
   our class hierarchy data structure.  Both parameters are trees
   representing the class types, i.e. RECORD_TYPE trees.
   DERIVED_CLASS can be the same as BASE_CLASS.  */

static void
update_class_hierarchy_information (tree base_class,
                                    tree derived_class)
{
  struct vtv_graph_node *base_node = find_graph_node (base_class);
  struct vtv_graph_node *derived_node = find_graph_node (derived_class);

  add_hierarchy_pair (base_node, derived_node);
}


static void
write_out_vtv_count_data (void)
{
  static int vtv_count_log_fd = -1;
  char buffer[1024];
  int unused_vtbl_map_vars = 0;
  int bytes_written __attribute__ ((unused));
  char *file_name = get_log_file_name ("vtv_count_data.log");

  if (vtv_count_log_fd == -1)
    vtv_count_log_fd = open (file_name,
                             O_WRONLY | O_APPEND | O_CREAT, S_IRWXU);
  if (vtv_count_log_fd == -1)
    {
      warning_at (UNKNOWN_LOCATION, 0,
		  "unable to open log file %<vtv_count_data.log%>: %m");
      return;
    }

  for (unsigned i = 0; i < num_vtable_map_nodes; ++i)
    {
      struct vtbl_map_node *current = vtbl_map_nodes_vec[i];
      if (!current->is_used
          && current->registered.size() == 0)
        unused_vtbl_map_vars++;
    }

  snprintf (buffer, sizeof (buffer), "%s %d %d %d %d %d\n",
            main_input_filename, total_num_virtual_calls,
            total_num_verified_vcalls, num_calls_to_regset,
            num_calls_to_regpair, unused_vtbl_map_vars);

  bytes_written = write (vtv_count_log_fd, buffer, strlen (buffer));
}

/* This function calls register_all_pairs, which actually generates
   all the calls to __VLTRegisterPair (in the verification constructor
   init function).  It also generates the calls to
   __VLTChangePermission, if the verification constructor init
   function is going into the preinit array.  INIT_ROUTINE_BODY is
   the body of our constructior initialization function, to which we
   add our function calls.*/

bool
vtv_register_class_hierarchy_information (tree init_routine_body)
{
  bool registered_something = false;
 
  init_functions ();

  if (num_vtable_map_nodes == 0)
    return false;

  /* Add class hierarchy pairs to the vtable map data structure.  */
  registered_something = register_all_pairs (init_routine_body);

  if (flag_vtv_counts)
    write_out_vtv_count_data ();

  return registered_something;
}


/* Generate the special constructor function that calls
   __VLTChangePermission and __VLTRegisterPairs, and give it a very
   high initialization priority.  */

void
vtv_generate_init_routine (void)
{
  tree init_routine_body;
  bool vtable_classes_found = false;

  push_lang_context (lang_name_c);

  /* The priority for this init function (constructor) is carefully
     chosen so that it will happen after the calls to unprotect the
     memory used for vtable verification and before the memory is
     protected again.  */
  init_routine_body = vtv_start_verification_constructor_init_function ();

  vtable_classes_found =
                 vtv_register_class_hierarchy_information (init_routine_body);

  if (vtable_classes_found)
    {
      tree vtv_fndecl =
        vtv_finish_verification_constructor_init_function (init_routine_body);
      TREE_STATIC (vtv_fndecl) = 1;
      TREE_USED (vtv_fndecl) = 1;
      DECL_PRESERVE_P (vtv_fndecl) = 1;
      if (flag_vtable_verify == VTV_PREINIT_PRIORITY)
        DECL_STATIC_CONSTRUCTOR (vtv_fndecl) = 0;

      gimplify_function_tree (vtv_fndecl);
      cgraph_add_new_function (vtv_fndecl, false);

      cgraph_process_new_functions ();

      if (flag_vtable_verify == VTV_PREINIT_PRIORITY)
        assemble_vtv_preinit_initializer (vtv_fndecl);

    }
  pop_lang_context ();
}

/* This funtion takes a tree containing a class type (BASE_TYPE), and
   it either finds the existing vtbl_map_node for that class in our
   data structure, or it creates a new node and adds it to the data
   structure if there is not one for the class already.  As part of
   this process it also creates the global vtable map variable for the
   class.  */

struct vtbl_map_node *
vtable_find_or_create_map_decl (tree base_type)
{
  char *var_name = NULL;
  struct vtbl_map_node *vtable_map_node = NULL;

  /* Verify the type has an associated vtable.  */
  if (!TYPE_BINFO (base_type) || !BINFO_VTABLE (TYPE_BINFO (base_type)))
    return NULL;

  /* Create map lookup symbol for base class */
  var_name = get_mangled_vtable_map_var_name (base_type);

  /* We've already created the variable; just look it.  */
  vtable_map_node = vtbl_map_get_node (TYPE_MAIN_VARIANT (base_type));

  if (!vtable_map_node || (vtable_map_node->vtbl_map_decl == NULL_TREE))
    {
      /* If we haven't already created the *__vtable_map global
         variable for this class, do so now, and add it to the
         varpool, to make sure it gets saved and written out.  */

      tree var_decl = NULL;
      tree var_type = build_pointer_type (void_type_node);
      tree initial_value = integer_zero_node;

      var_decl  = build_decl (UNKNOWN_LOCATION, VAR_DECL,
                              get_identifier (var_name), var_type);

      DECL_EXTERNAL (var_decl) = 0;
      TREE_STATIC (var_decl) = 1;
      DECL_VISIBILITY (var_decl) = VISIBILITY_HIDDEN;
      SET_DECL_ASSEMBLER_NAME (var_decl, get_identifier (var_name));
      DECL_ARTIFICIAL (var_decl) = 1;
      /* We cannot mark this variable as read-only because we want to be
         able to write to it at runtime.  */
      TREE_READONLY (var_decl) = 0;
      DECL_IGNORED_P (var_decl) = 1;
      DECL_PRESERVE_P (var_decl) = 1;

      /* Put these mmap variables in thr .vtable_map_vars section, so
         we can find and protect them.  */

      set_decl_section_name (var_decl, ".vtable_map_vars");
      symtab_get_node (var_decl)->implicit_section = true;
      DECL_INITIAL (var_decl) = initial_value;

      comdat_linkage (var_decl);

      varpool_finalize_decl (var_decl);
      if (!vtable_map_node)
        vtable_map_node =
                   find_or_create_vtbl_map_node (TYPE_MAIN_VARIANT (base_type));
      if (vtable_map_node->vtbl_map_decl == NULL_TREE)
        vtable_map_node->vtbl_map_decl = var_decl;
    }

  gcc_assert (vtable_map_node);
  return vtable_map_node;
}

/* This function is used to build up our class hierarchy data for a
   particular class.  TYPE is the record_type tree node for the
   class.  */

static void
vtv_insert_single_class_info (tree type)
{
  if (flag_vtable_verify)
    {
      tree binfo =  TYPE_BINFO (type);
      tree base_binfo;
      struct vtbl_map_node *own_map;
      int i;

      /* First make sure to create the map for this record type.  */
      own_map = vtable_find_or_create_map_decl (type);
      if (own_map == NULL)
        return;

      /* Go through the list of all base classes for the current
         (derived) type, make sure the *__vtable_map global variable
         for the base class exists, and add the base class/derived
         class pair to the class hierarchy information we are
         accumulating (for vtable pointer verification).  */
      for (i = 0; BINFO_BASE_ITERATE (binfo, i, base_binfo); i++)
        {
          tree tree_val = BINFO_TYPE (base_binfo);
          struct vtbl_map_node *vtable_map_node = NULL;

          vtable_map_node = vtable_find_or_create_map_decl (tree_val);

          if (vtable_map_node != NULL)
            update_class_hierarchy_information (tree_val, type);
        }
    }
}

/* This function adds classes we are interested in to a list of
   classes.  RECORD is the record_type node for the class we are
   adding to the list.  */

void
vtv_save_class_info (tree record)
{
  if (!flag_vtable_verify || TREE_CODE (record) == UNION_TYPE)
    return;

  if (!vlt_saved_class_info)
    vec_alloc (vlt_saved_class_info, 10);

  gcc_assert (TREE_CODE (record) == RECORD_TYPE);

  vec_safe_push (vlt_saved_class_info, record);
}


/* This function goes through the list of classes we saved and calls
   vtv_insert_single_class_info on each one, to build up our class
   hierarchy data structure.  */

void
vtv_recover_class_info (void)
{
  tree current_class;
  unsigned i;

  if (vlt_saved_class_info)
    {
      for (i = 0; i < vlt_saved_class_info->length(); ++i)
        {
          current_class = (*vlt_saved_class_info)[i];
          gcc_assert (TREE_CODE (current_class) == RECORD_TYPE);
          vtv_insert_single_class_info (current_class);
        }
    }
}

#include "gt-cp-vtable-class-hierarchy.h"
