/****************************************************************************
 *                                                                          *
 *                         GNAT COMPILER COMPONENTS                         *
 *                                                                          *
 *                                U T I L S                                 *
 *                                                                          *
 *                          C Implementation File                           *
 *                                                                          *
 *          Copyright (C) 1992-2005, Free Software Foundation, Inc.         *
 *                                                                          *
 * GNAT is free software;  you can  redistribute it  and/or modify it under *
 * terms of the  GNU General Public License as published  by the Free Soft- *
 * ware  Foundation;  either version 2,  or (at your option) any later ver- *
 * sion.  GNAT is distributed in the hope that it will be useful, but WITH- *
 * OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY *
 * or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License *
 * for  more details.  You should have  received  a copy of the GNU General *
 * Public License  distributed with GNAT;  see file COPYING.  If not, write *
 * to  the  Free Software Foundation,  51  Franklin  Street,  Fifth  Floor, *
 * Boston, MA 02110-1301, USA.                                              *
 *                                                                          *
 * GNAT was originally developed  by the GNAT team at  New York University. *
 * Extensive contributions were provided by Ada Core Technologies Inc.      *
 *                                                                          *
 ****************************************************************************/

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "tree.h"
#include "flags.h"
#include "defaults.h"
#include "toplev.h"
#include "output.h"
#include "ggc.h"
#include "debug.h"
#include "convert.h"
#include "target.h"
#include "function.h"
#include "cgraph.h"
#include "tree-inline.h"
#include "tree-gimple.h"
#include "tree-dump.h"

#include "ada.h"
#include "types.h"
#include "atree.h"
#include "elists.h"
#include "namet.h"
#include "nlists.h"
#include "stringt.h"
#include "uintp.h"
#include "fe.h"
#include "sinfo.h"
#include "einfo.h"
#include "ada-tree.h"
#include "gigi.h"

#ifndef MAX_FIXED_MODE_SIZE
#define MAX_FIXED_MODE_SIZE GET_MODE_BITSIZE (DImode)
#endif

#ifndef MAX_BITS_PER_WORD
#define MAX_BITS_PER_WORD  BITS_PER_WORD
#endif

/* If nonzero, pretend we are allocating at global level.  */
int force_global;

/* Tree nodes for the various types and decls we create.  */
tree gnat_std_decls[(int) ADT_LAST];

/* Functions to call for each of the possible raise reasons.  */
tree gnat_raise_decls[(int) LAST_REASON_CODE + 1];

/* List of functions called automatically at the beginning and
   end of execution, on targets without .ctors/.dtors sections.  */
tree static_ctors;
tree static_dtors;

/* Associates a GNAT tree node to a GCC tree node. It is used in
   `save_gnu_tree', `get_gnu_tree' and `present_gnu_tree'. See documentation
   of `save_gnu_tree' for more info.  */
static GTY((length ("max_gnat_nodes"))) tree *associate_gnat_to_gnu;

/* This variable keeps a table for types for each precision so that we only
   allocate each of them once. Signed and unsigned types are kept separate.

   Note that these types are only used when fold-const requests something
   special.  Perhaps we should NOT share these types; we'll see how it
   goes later.  */
static GTY(()) tree signed_and_unsigned_types[2 * MAX_BITS_PER_WORD + 1][2];

/* Likewise for float types, but record these by mode.  */
static GTY(()) tree float_types[NUM_MACHINE_MODES];

/* For each binding contour we allocate a binding_level structure to indicate
   the binding depth.  */

struct gnat_binding_level GTY((chain_next ("%h.chain")))
{
  /* The binding level containing this one (the enclosing binding level). */
  struct gnat_binding_level *chain;
  /* The BLOCK node for this level.  */
  tree block;
  /* If nonzero, the setjmp buffer that needs to be updated for any
     variable-sized definition within this context.  */
  tree jmpbuf_decl;
};

/* The binding level currently in effect.  */
static GTY(()) struct gnat_binding_level *current_binding_level;

/* A chain of gnat_binding_level structures awaiting reuse.  */
static GTY((deletable)) struct gnat_binding_level *free_binding_level;

/* A chain of unused BLOCK nodes. */
static GTY((deletable)) tree free_block_chain;

struct language_function GTY(())
{
  int unused;
};

static void gnat_install_builtins (void);
static tree merge_sizes (tree, tree, tree, bool, bool);
static tree compute_related_constant (tree, tree);
static tree split_plus (tree, tree *);
static bool value_zerop (tree);
static void gnat_gimplify_function (tree);
static tree float_type_for_precision (int, enum machine_mode);
static tree convert_to_fat_pointer (tree, tree);
static tree convert_to_thin_pointer (tree, tree);
static tree make_descriptor_field (const char *,tree, tree, tree);
static bool value_factor_p (tree, HOST_WIDE_INT);
static bool potential_alignment_gap (tree, tree, tree);

/* Initialize the association of GNAT nodes to GCC trees.  */

void
init_gnat_to_gnu (void)
{
  associate_gnat_to_gnu
    = (tree *) ggc_alloc_cleared (max_gnat_nodes * sizeof (tree));
}

/* GNAT_ENTITY is a GNAT tree node for an entity.   GNU_DECL is the GCC tree
   which is to be associated with GNAT_ENTITY. Such GCC tree node is always
   a ..._DECL node.  If NO_CHECK is nonzero, the latter check is suppressed.

   If GNU_DECL is zero, a previous association is to be reset.  */

void
save_gnu_tree (Entity_Id gnat_entity, tree gnu_decl, bool no_check)
{
  /* Check that GNAT_ENTITY is not already defined and that it is being set
     to something which is a decl.  Raise gigi 401 if not.  Usually, this
     means GNAT_ENTITY is defined twice, but occasionally is due to some
     Gigi problem.  */
  gcc_assert (!gnu_decl
	      || (!associate_gnat_to_gnu[gnat_entity - First_Node_Id]
		  && (no_check || DECL_P (gnu_decl))));
  associate_gnat_to_gnu[gnat_entity - First_Node_Id] = gnu_decl;
}

/* GNAT_ENTITY is a GNAT tree node for a defining identifier.
   Return the ..._DECL node that was associated with it.  If there is no tree
   node associated with GNAT_ENTITY, abort.

   In some cases, such as delayed elaboration or expressions that need to
   be elaborated only once, GNAT_ENTITY is really not an entity.  */

tree
get_gnu_tree (Entity_Id gnat_entity)
{
  gcc_assert (associate_gnat_to_gnu[gnat_entity - First_Node_Id]);
  return associate_gnat_to_gnu[gnat_entity - First_Node_Id];
}

/* Return nonzero if a GCC tree has been associated with GNAT_ENTITY.  */

bool
present_gnu_tree (Entity_Id gnat_entity)
{
  return (associate_gnat_to_gnu[gnat_entity - First_Node_Id]) != 0;
}


/* Return non-zero if we are currently in the global binding level.  */

int
global_bindings_p (void)
{
  return ((force_global || !current_function_decl) ? -1 : 0);
}

/* Enter a new binding level. */

void
gnat_pushlevel ()
{
  struct gnat_binding_level *newlevel = NULL;

  /* Reuse a struct for this binding level, if there is one.  */
  if (free_binding_level)
    {
      newlevel = free_binding_level;
      free_binding_level = free_binding_level->chain;
    }
  else
    newlevel
      = (struct gnat_binding_level *)
	ggc_alloc (sizeof (struct gnat_binding_level));

  /* Use a free BLOCK, if any; otherwise, allocate one.  */
  if (free_block_chain)
    {
      newlevel->block = free_block_chain;
      free_block_chain = TREE_CHAIN (free_block_chain);
      TREE_CHAIN (newlevel->block) = NULL_TREE;
    }
  else
    newlevel->block = make_node (BLOCK);

  /* Point the BLOCK we just made to its parent.  */
  if (current_binding_level)
    BLOCK_SUPERCONTEXT (newlevel->block) = current_binding_level->block;

  BLOCK_VARS (newlevel->block) = BLOCK_SUBBLOCKS (newlevel->block) = NULL_TREE;
  TREE_USED (newlevel->block) = 1;

  /* Add this level to the front of the chain (stack) of levels that are
     active.  */
  newlevel->chain = current_binding_level;
  newlevel->jmpbuf_decl = NULL_TREE;
  current_binding_level = newlevel;
}

/* Set SUPERCONTEXT of the BLOCK for the current binding level to FNDECL
   and point FNDECL to this BLOCK.  */

void
set_current_block_context (tree fndecl)
{
  BLOCK_SUPERCONTEXT (current_binding_level->block) = fndecl;
  DECL_INITIAL (fndecl) = current_binding_level->block;
}

/* Set the jmpbuf_decl for the current binding level to DECL.  */

void
set_block_jmpbuf_decl (tree decl)
{
  current_binding_level->jmpbuf_decl = decl;
}

/* Get the jmpbuf_decl, if any, for the current binding level.  */

tree
get_block_jmpbuf_decl ()
{
  return current_binding_level->jmpbuf_decl;
}

/* Exit a binding level. Set any BLOCK into the current code group.  */

void
gnat_poplevel ()
{
  struct gnat_binding_level *level = current_binding_level;
  tree block = level->block;

  BLOCK_VARS (block) = nreverse (BLOCK_VARS (block));
  BLOCK_SUBBLOCKS (block) = nreverse (BLOCK_SUBBLOCKS (block));

  /* If this is a function-level BLOCK don't do anything.  Otherwise, if there
     are no variables free the block and merge its subblocks into those of its
     parent block. Otherwise, add it to the list of its parent.  */
  if (TREE_CODE (BLOCK_SUPERCONTEXT (block)) == FUNCTION_DECL)
    ;
  else if (BLOCK_VARS (block) == NULL_TREE)
    {
      BLOCK_SUBBLOCKS (level->chain->block)
	= chainon (BLOCK_SUBBLOCKS (block),
		   BLOCK_SUBBLOCKS (level->chain->block));
      TREE_CHAIN (block) = free_block_chain;
      free_block_chain = block;
    }
  else
    {
      TREE_CHAIN (block) = BLOCK_SUBBLOCKS (level->chain->block);
      BLOCK_SUBBLOCKS (level->chain->block) = block;
      TREE_USED (block) = 1;
      set_block_for_group (block);
    }

  /* Free this binding structure.  */
  current_binding_level = level->chain;
  level->chain = free_binding_level;
  free_binding_level = level;
}

/* Insert BLOCK at the end of the list of subblocks of the
   current binding level.  This is used when a BIND_EXPR is expanded,
   to handle the BLOCK node inside the BIND_EXPR.  */

void
insert_block (tree block)
{
  TREE_USED (block) = 1;
  TREE_CHAIN (block) = BLOCK_SUBBLOCKS (current_binding_level->block);
  BLOCK_SUBBLOCKS (current_binding_level->block) = block;
}

/* Records a ..._DECL node DECL as belonging to the current lexical scope
   and uses GNAT_NODE for location information and propagating flags.  */

void
gnat_pushdecl (tree decl, Node_Id gnat_node)
{
  /* If at top level, there is no context. But PARM_DECLs always go in the
     level of its function.  */
  if (global_bindings_p () && TREE_CODE (decl) != PARM_DECL)
    DECL_CONTEXT (decl) = 0;
  else
    {
      DECL_CONTEXT (decl) = current_function_decl;

      /* Functions imported in another function are not really nested.  */
      if (TREE_CODE (decl) == FUNCTION_DECL && TREE_PUBLIC (decl))
	DECL_NO_STATIC_CHAIN (decl) = 1;
    }

  TREE_NO_WARNING (decl) = (gnat_node == Empty || Warnings_Off (gnat_node));

  /* Set the location of DECL and emit a declaration for it.  */
  if (Present (gnat_node))
    Sloc_to_locus (Sloc (gnat_node), &DECL_SOURCE_LOCATION (decl));
  add_decl_expr (decl, gnat_node);

  /* Put the declaration on the list.  The list of declarations is in reverse
     order. The list will be reversed later.  We don't do this for global
     variables.  Also, don't put TYPE_DECLs for UNCONSTRAINED_ARRAY_TYPE into
     the list.  They will cause trouble with the debugger and aren't needed
     anyway.  */
  if (!global_bindings_p ()
      && (TREE_CODE (decl) != TYPE_DECL
	  || TREE_CODE (TREE_TYPE (decl)) != UNCONSTRAINED_ARRAY_TYPE))
    {
      TREE_CHAIN (decl) = BLOCK_VARS (current_binding_level->block);
      BLOCK_VARS (current_binding_level->block) = decl;
    }

  /* For the declaration of a type, set its name if it either is not already
     set, was set to an IDENTIFIER_NODE, indicating an internal name,
     or if the previous type name was not derived from a source name.
     We'd rather have the type named with a real name and all the pointer
     types to the same object have the same POINTER_TYPE node.  Code in this
     function in c-decl.c makes a copy of the type node here, but that may
     cause us trouble with incomplete types, so let's not try it (at least
     for now).  */

  if (TREE_CODE (decl) == TYPE_DECL
      && DECL_NAME (decl)
      && (!TYPE_NAME (TREE_TYPE (decl))
	  || TREE_CODE (TYPE_NAME (TREE_TYPE (decl))) == IDENTIFIER_NODE
	  || (TREE_CODE (TYPE_NAME (TREE_TYPE (decl))) == TYPE_DECL
	      && DECL_ARTIFICIAL (TYPE_NAME (TREE_TYPE (decl)))
	      && !DECL_ARTIFICIAL (decl))))
    TYPE_NAME (TREE_TYPE (decl)) = decl;

  /*  if (TREE_CODE (decl) != CONST_DECL)
      rest_of_decl_compilation (decl, global_bindings_p (), 0); */
}

/* Do little here.  Set up the standard declarations later after the
   front end has been run.  */

void
gnat_init_decl_processing (void)
{
  input_line = 0;

  /* Make the binding_level structure for global names.  */
  current_function_decl = 0;
  current_binding_level = 0;
  free_binding_level = 0;
  gnat_pushlevel ();

  build_common_tree_nodes (true, true);

  /* In Ada, we use a signed type for SIZETYPE.  Use the signed type
     corresponding to the size of Pmode.  In most cases when ptr_mode and
     Pmode differ, C will use the width of ptr_mode as sizetype.  But we get
     far better code using the width of Pmode.  Make this here since we need
     this before we can expand the GNAT types.  */
  size_type_node = gnat_type_for_size (GET_MODE_BITSIZE (Pmode), 0);
  set_sizetype (size_type_node);
  build_common_tree_nodes_2 (0);

  /* Give names and make TYPE_DECLs for common types.  */
  gnat_pushdecl (build_decl (TYPE_DECL, get_identifier (SIZE_TYPE), sizetype),
		 Empty);
  gnat_pushdecl (build_decl (TYPE_DECL, get_identifier ("integer"),
			     integer_type_node),
		 Empty);
  gnat_pushdecl (build_decl (TYPE_DECL, get_identifier ("unsigned char"),
			     char_type_node),
		 Empty);
  gnat_pushdecl (build_decl (TYPE_DECL, get_identifier ("long integer"),
			     long_integer_type_node),
		 Empty);

  ptr_void_type_node = build_pointer_type (void_type_node);

  gnat_install_builtins ();
}

/* Install the builtin functions the middle-end needs.  */

static void
gnat_install_builtins ()
{
  /* Builtins used by generic optimizers.  */
  build_common_builtin_nodes ();

  /* Target specific builtins, such as the AltiVec family on ppc.  */
  targetm.init_builtins ();
}

/* Create the predefined scalar types such as `integer_type_node' needed
   in the gcc back-end and initialize the global binding level.  */

void
init_gigi_decls (tree long_long_float_type, tree exception_type)
{
  tree endlink, decl;
  unsigned int i;

  /* Set the types that GCC and Gigi use from the front end.  We would like
     to do this for char_type_node, but it needs to correspond to the C
     char type.  */
  if (TREE_CODE (TREE_TYPE (long_long_float_type)) == INTEGER_TYPE)
    {
      /* In this case, the builtin floating point types are VAX float,
	 so make up a type for use.  */
      longest_float_type_node = make_node (REAL_TYPE);
      TYPE_PRECISION (longest_float_type_node) = LONG_DOUBLE_TYPE_SIZE;
      layout_type (longest_float_type_node);
      create_type_decl (get_identifier ("longest float type"),
			longest_float_type_node, NULL, false, true, Empty);
    }
  else
    longest_float_type_node = TREE_TYPE (long_long_float_type);

  except_type_node = TREE_TYPE (exception_type);

  unsigned_type_node = gnat_type_for_size (INT_TYPE_SIZE, 1);
  create_type_decl (get_identifier ("unsigned int"), unsigned_type_node,
		    NULL, false, true, Empty);

  void_type_decl_node = create_type_decl (get_identifier ("void"),
					  void_type_node, NULL, false, true,
					  Empty);

  void_ftype = build_function_type (void_type_node, NULL_TREE);
  ptr_void_ftype = build_pointer_type (void_ftype);

  /* Now declare runtime functions. */
  endlink = tree_cons (NULL_TREE, void_type_node, NULL_TREE);

  /* malloc is a function declaration tree for a function to allocate
     memory.  */
  malloc_decl = create_subprog_decl (get_identifier ("__gnat_malloc"),
				     NULL_TREE,
				     build_function_type (ptr_void_type_node,
							  tree_cons (NULL_TREE,
								     sizetype,
								     endlink)),
				     NULL_TREE, false, true, true, NULL,
				     Empty);

  /* free is a function declaration tree for a function to free memory.  */
  free_decl
    = create_subprog_decl (get_identifier ("__gnat_free"), NULL_TREE,
			   build_function_type (void_type_node,
						tree_cons (NULL_TREE,
							   ptr_void_type_node,
							   endlink)),
			   NULL_TREE, false, true, true, NULL, Empty);

  /* Make the types and functions used for exception processing.    */
  jmpbuf_type
    = build_array_type (gnat_type_for_mode (Pmode, 0),
			build_index_type (build_int_cst (NULL_TREE, 5)));
  create_type_decl (get_identifier ("JMPBUF_T"), jmpbuf_type, NULL,
		    false, true, Empty);
  jmpbuf_ptr_type = build_pointer_type (jmpbuf_type);

  /* Functions to get and set the jumpbuf pointer for the current thread.  */
  get_jmpbuf_decl
    = create_subprog_decl
    (get_identifier ("system__soft_links__get_jmpbuf_address_soft"),
     NULL_TREE, build_function_type (jmpbuf_ptr_type, NULL_TREE),
     NULL_TREE, false, true, true, NULL, Empty);

  set_jmpbuf_decl
    = create_subprog_decl
    (get_identifier ("system__soft_links__set_jmpbuf_address_soft"),
     NULL_TREE,
     build_function_type (void_type_node,
			  tree_cons (NULL_TREE, jmpbuf_ptr_type, endlink)),
     NULL_TREE, false, true, true, NULL, Empty);

  /* Function to get the current exception.  */
  get_excptr_decl
    = create_subprog_decl
    (get_identifier ("system__soft_links__get_gnat_exception"),
     NULL_TREE,
     build_function_type (build_pointer_type (except_type_node), NULL_TREE),
     NULL_TREE, false, true, true, NULL, Empty);

  /* Functions that raise exceptions. */
  raise_nodefer_decl
    = create_subprog_decl
      (get_identifier ("__gnat_raise_nodefer_with_msg"), NULL_TREE,
       build_function_type (void_type_node,
			    tree_cons (NULL_TREE,
				       build_pointer_type (except_type_node),
				       endlink)),
       NULL_TREE, false, true, true, NULL, Empty);

  /* Dummy objects to materialize "others" and "all others" in the exception
     tables.  These are exported by a-exexpr.adb, so see this unit for the
     types to use.  */

  others_decl
    = create_var_decl (get_identifier ("OTHERS"),
		       get_identifier ("__gnat_others_value"),
		       integer_type_node, 0, 1, 0, 1, 1, 0, Empty);

  all_others_decl
    = create_var_decl (get_identifier ("ALL_OTHERS"),
		       get_identifier ("__gnat_all_others_value"),
		       integer_type_node, 0, 1, 0, 1, 1, 0, Empty);

  /* Hooks to call when entering/leaving an exception handler.  */
  begin_handler_decl
    = create_subprog_decl (get_identifier ("__gnat_begin_handler"), NULL_TREE,
			   build_function_type (void_type_node,
						tree_cons (NULL_TREE,
							   ptr_void_type_node,
							   endlink)),
			   NULL_TREE, false, true, true, NULL, Empty);

  end_handler_decl
    = create_subprog_decl (get_identifier ("__gnat_end_handler"), NULL_TREE,
			   build_function_type (void_type_node,
						tree_cons (NULL_TREE,
							   ptr_void_type_node,
							   endlink)),
			   NULL_TREE, false, true, true, NULL, Empty);

  /* If in no exception handlers mode, all raise statements are redirected to
     __gnat_last_chance_handler. No need to redefine raise_nodefer_decl, since
     this procedure will never be called in this mode.  */
  if (No_Exception_Handlers_Set ())
    {
      decl
	= create_subprog_decl
	  (get_identifier ("__gnat_last_chance_handler"), NULL_TREE,
	   build_function_type (void_type_node,
				tree_cons (NULL_TREE,
					   build_pointer_type (char_type_node),
					   tree_cons (NULL_TREE,
						      integer_type_node,
						      endlink))),
	   NULL_TREE, false, true, true, NULL, Empty);

      for (i = 0; i < ARRAY_SIZE (gnat_raise_decls); i++)
	gnat_raise_decls[i] = decl;
    }
  else
    /* Otherwise, make one decl for each exception reason.  */
    for (i = 0; i < ARRAY_SIZE (gnat_raise_decls); i++)
      {
	char name[17];

	sprintf (name, "__gnat_rcheck_%.2d", i);
	gnat_raise_decls[i]
	  = create_subprog_decl
	    (get_identifier (name), NULL_TREE,
	     build_function_type (void_type_node,
				  tree_cons (NULL_TREE,
					     build_pointer_type
					     (char_type_node),
					     tree_cons (NULL_TREE,
							integer_type_node,
							endlink))),
	     NULL_TREE, false, true, true, NULL, Empty);
      }

  /* Indicate that these never return.  */
  TREE_THIS_VOLATILE (raise_nodefer_decl) = 1;
  TREE_SIDE_EFFECTS (raise_nodefer_decl) = 1;
  TREE_TYPE (raise_nodefer_decl)
    = build_qualified_type (TREE_TYPE (raise_nodefer_decl),
			    TYPE_QUAL_VOLATILE);

  for (i = 0; i < ARRAY_SIZE (gnat_raise_decls); i++)
    {
      TREE_THIS_VOLATILE (gnat_raise_decls[i]) = 1;
      TREE_SIDE_EFFECTS (gnat_raise_decls[i]) = 1;
      TREE_TYPE (gnat_raise_decls[i])
	= build_qualified_type (TREE_TYPE (gnat_raise_decls[i]),
				TYPE_QUAL_VOLATILE);
    }

  /* setjmp returns an integer and has one operand, which is a pointer to
     a jmpbuf.  */
  setjmp_decl
    = create_subprog_decl
      (get_identifier ("__builtin_setjmp"), NULL_TREE,
       build_function_type (integer_type_node,
			    tree_cons (NULL_TREE,  jmpbuf_ptr_type, endlink)),
       NULL_TREE, false, true, true, NULL, Empty);

  DECL_BUILT_IN_CLASS (setjmp_decl) = BUILT_IN_NORMAL;
  DECL_FUNCTION_CODE (setjmp_decl) = BUILT_IN_SETJMP;

  /* update_setjmp_buf updates a setjmp buffer from the current stack pointer
     address.  */
  update_setjmp_buf_decl
    = create_subprog_decl
      (get_identifier ("__builtin_update_setjmp_buf"), NULL_TREE,
       build_function_type (void_type_node,
			    tree_cons (NULL_TREE,  jmpbuf_ptr_type, endlink)),
       NULL_TREE, false, true, true, NULL, Empty);

  DECL_BUILT_IN_CLASS (update_setjmp_buf_decl) = BUILT_IN_NORMAL;
  DECL_FUNCTION_CODE (update_setjmp_buf_decl) = BUILT_IN_UPDATE_SETJMP_BUF;

  main_identifier_node = get_identifier ("main");
}

/* Given a record type (RECORD_TYPE) and a chain of FIELD_DECL nodes
   (FIELDLIST), finish constructing the record or union type.  If HAS_REP is
   true, this record has a rep clause; don't call layout_type but merely set
   the size and alignment ourselves.  If DEFER_DEBUG is true, do not call
   the debugging routines on this type; it will be done later. */

void
finish_record_type (tree record_type, tree fieldlist, bool has_rep,
                    bool defer_debug)
{
  enum tree_code code = TREE_CODE (record_type);
  tree ada_size = bitsize_zero_node;
  tree size = bitsize_zero_node;
  bool var_size = false;
  bool had_size = TYPE_SIZE (record_type) != 0;
  bool had_size_unit = TYPE_SIZE_UNIT (record_type) != 0;
  tree field;

  TYPE_FIELDS (record_type) = fieldlist;
  TYPE_STUB_DECL (record_type)
    = build_decl (TYPE_DECL, NULL_TREE, record_type);

  /* We don't need both the typedef name and the record name output in
     the debugging information, since they are the same.  */
  DECL_ARTIFICIAL (TYPE_STUB_DECL (record_type)) = 1;

  /* Globally initialize the record first.  If this is a rep'ed record,
     that just means some initializations; otherwise, layout the record.  */

  if (has_rep)
    {
      TYPE_ALIGN (record_type) = MAX (BITS_PER_UNIT, TYPE_ALIGN (record_type));
      TYPE_MODE (record_type) = BLKmode;

      if (!had_size_unit)
	TYPE_SIZE_UNIT (record_type) = size_zero_node;
      if (!had_size)
	TYPE_SIZE (record_type) = bitsize_zero_node;

      /* For all-repped records with a size specified, lay the QUAL_UNION_TYPE
	 out just like a UNION_TYPE, since the size will be fixed.  */
      else if (code == QUAL_UNION_TYPE)
	code = UNION_TYPE;
    }
  else
    {
      /* Ensure there isn't a size already set.  There can be in an error
	 case where there is a rep clause but all fields have errors and
	 no longer have a position.  */
      TYPE_SIZE (record_type) = 0;
      layout_type (record_type);
    }

  /* At this point, the position and size of each field is known.  It was
     either set before entry by a rep clause, or by laying out the type above.

     We now run a pass over the fields (in reverse order for QUAL_UNION_TYPEs)
     to compute the Ada size; the GCC size and alignment (for rep'ed records
     that are not padding types); and the mode (for rep'ed records).  We also
     clear the DECL_BIT_FIELD indication for the cases we know have not been
     handled yet, and adjust DECL_NONADDRESSABLE_P accordingly.  */

  if (code == QUAL_UNION_TYPE)
    fieldlist = nreverse (fieldlist);

  for (field = fieldlist; field; field = TREE_CHAIN (field))
    {
      tree pos = bit_position (field);

      tree type = TREE_TYPE (field);
      tree this_size = DECL_SIZE (field);
      tree this_ada_size = DECL_SIZE (field);

      /* We need to make an XVE/XVU record if any field has variable size,
	 whether or not the record does.  For example, if we have a union,
	 it may be that all fields, rounded up to the alignment, have the
	 same size, in which case we'll use that size.  But the debug
	 output routines (except Dwarf2) won't be able to output the fields,
	 so we need to make the special record.  */
      if (TREE_CODE (this_size) != INTEGER_CST)
	var_size = true;

      if ((TREE_CODE (type) == RECORD_TYPE || TREE_CODE (type) == UNION_TYPE
	  || TREE_CODE (type) == QUAL_UNION_TYPE)
	  && !TYPE_IS_FAT_POINTER_P (type)
	  && !TYPE_CONTAINS_TEMPLATE_P (type)
	  && TYPE_ADA_SIZE (type))
	this_ada_size = TYPE_ADA_SIZE (type);

      /* Clear DECL_BIT_FIELD for the cases layout_decl does not handle.  */
      if (DECL_BIT_FIELD (field) && !STRICT_ALIGNMENT
	  && value_factor_p (pos, BITS_PER_UNIT)
	  && operand_equal_p (this_size, TYPE_SIZE (type), 0))
	DECL_BIT_FIELD (field) = 0;

      /* If we still have DECL_BIT_FIELD set at this point, we know the field
	 is technically not addressable.  Except that it can actually be
	 addressed if the field is BLKmode and happens to be properly
	 aligned.  */
      DECL_NONADDRESSABLE_P (field)
	|= DECL_BIT_FIELD (field) && DECL_MODE (field) != BLKmode;

      if (has_rep && !DECL_BIT_FIELD (field))
	TYPE_ALIGN (record_type)
	  = MAX (TYPE_ALIGN (record_type), DECL_ALIGN (field));

      switch (code)
	{
	case UNION_TYPE:
	  ada_size = size_binop (MAX_EXPR, ada_size, this_ada_size);
	  size = size_binop (MAX_EXPR, size, this_size);
	  break;

	case QUAL_UNION_TYPE:
	  ada_size
	    = fold (build3 (COND_EXPR, bitsizetype, DECL_QUALIFIER (field),
			    this_ada_size, ada_size));
	  size = fold (build3 (COND_EXPR, bitsizetype, DECL_QUALIFIER (field),
			       this_size, size));
	  break;

	case RECORD_TYPE:
	  /* Since we know here that all fields are sorted in order of
	     increasing bit position, the size of the record is one
	     higher than the ending bit of the last field processed
	     unless we have a rep clause, since in that case we might
	     have a field outside a QUAL_UNION_TYPE that has a higher ending
	     position.  So use a MAX in that case.  Also, if this field is a
	     QUAL_UNION_TYPE, we need to take into account the previous size in
	     the case of empty variants.  */
	  ada_size
	    = merge_sizes (ada_size, pos, this_ada_size,
			   TREE_CODE (type) == QUAL_UNION_TYPE, has_rep);
	  size = merge_sizes (size, pos, this_size,
			      TREE_CODE (type) == QUAL_UNION_TYPE, has_rep);
	  break;

	default:
	  gcc_unreachable ();
	}
    }

  if (code == QUAL_UNION_TYPE)
    nreverse (fieldlist);

  /* If this is a padding record, we never want to make the size smaller than
     what was specified in it, if any.  */
  if (TREE_CODE (record_type) == RECORD_TYPE
      && TYPE_IS_PADDING_P (record_type) && TYPE_SIZE (record_type))
    size = TYPE_SIZE (record_type);

  /* Now set any of the values we've just computed that apply.  */
  if (!TYPE_IS_FAT_POINTER_P (record_type)
      && !TYPE_CONTAINS_TEMPLATE_P (record_type))
    SET_TYPE_ADA_SIZE (record_type, ada_size);

  if (has_rep)
    {
      tree size_unit
	= (had_size_unit ? TYPE_SIZE_UNIT (record_type)
	   : convert (sizetype, size_binop (CEIL_DIV_EXPR, size,
					    bitsize_unit_node)));

      TYPE_SIZE (record_type)
	= variable_size (round_up (size, TYPE_ALIGN (record_type)));
      TYPE_SIZE_UNIT (record_type)
	= variable_size (round_up (size_unit,
				   TYPE_ALIGN (record_type) / BITS_PER_UNIT));

      compute_record_mode (record_type);
    }

  if (!defer_debug)
    write_record_type_debug_info (record_type);
}

/* Output the debug information associated to a record type.  */

void
write_record_type_debug_info (tree record_type)
{
  tree fieldlist = TYPE_FIELDS (record_type);
  tree field;
  bool var_size = false;

  for (field = fieldlist; field; field = TREE_CHAIN (field))
    {
      /* We need to make an XVE/XVU record if any field has variable size,
	 whether or not the record does.  For example, if we have a union,
	 it may be that all fields, rounded up to the alignment, have the
	 same size, in which case we'll use that size.  But the debug
	 output routines (except Dwarf2) won't be able to output the fields,
	 so we need to make the special record.  */
      if (TREE_CODE (DECL_SIZE (field)) != INTEGER_CST)
	{
	  var_size = true;
	  break;
	}
    }

  /* If this record is of variable size, rename it so that the
     debugger knows it is and make a new, parallel, record
     that tells the debugger how the record is laid out.  See
     exp_dbug.ads.  But don't do this for records that are padding
     since they confuse GDB.  */
  if (var_size
      && !(TREE_CODE (record_type) == RECORD_TYPE
	   && TYPE_IS_PADDING_P (record_type)))
    {
      tree new_record_type
	= make_node (TREE_CODE (record_type) == QUAL_UNION_TYPE
		     ? UNION_TYPE : TREE_CODE (record_type));
      tree orig_name = TYPE_NAME (record_type);
      tree orig_id
	= (TREE_CODE (orig_name) == TYPE_DECL ? DECL_NAME (orig_name)
	   : orig_name);
      tree new_id
	= concat_id_with_name (orig_id,
			       TREE_CODE (record_type) == QUAL_UNION_TYPE
			       ? "XVU" : "XVE");
      tree last_pos = bitsize_zero_node;
      tree old_field;
      tree prev_old_field = 0;

      TYPE_NAME (new_record_type) = new_id;
      TYPE_ALIGN (new_record_type) = BIGGEST_ALIGNMENT;
      TYPE_STUB_DECL (new_record_type)
	= build_decl (TYPE_DECL, NULL_TREE, new_record_type);
      DECL_ARTIFICIAL (TYPE_STUB_DECL (new_record_type)) = 1;
      DECL_IGNORED_P (TYPE_STUB_DECL (new_record_type))
	= DECL_IGNORED_P (TYPE_STUB_DECL (record_type));
      TYPE_SIZE (new_record_type) = size_int (TYPE_ALIGN (record_type));
      TYPE_SIZE_UNIT (new_record_type)
	= size_int (TYPE_ALIGN (record_type) / BITS_PER_UNIT);

      /* Now scan all the fields, replacing each field with a new
	 field corresponding to the new encoding.  */
      for (old_field = TYPE_FIELDS (record_type); old_field;
	   old_field = TREE_CHAIN (old_field))
	{
	  tree field_type = TREE_TYPE (old_field);
	  tree field_name = DECL_NAME (old_field);
	  tree new_field;
	  tree curpos = bit_position (old_field);
	  bool var = false;
	  unsigned int align = 0;
	  tree pos;

	  /* See how the position was modified from the last position.

	  There are two basic cases we support: a value was added
	  to the last position or the last position was rounded to
	  a boundary and they something was added.  Check for the
	  first case first.  If not, see if there is any evidence
	  of rounding.  If so, round the last position and try
	  again.

	  If this is a union, the position can be taken as zero. */

	  if (TREE_CODE (new_record_type) == UNION_TYPE)
	    pos = bitsize_zero_node, align = 0;
	  else
	    pos = compute_related_constant (curpos, last_pos);

	  if (!pos && TREE_CODE (curpos) == MULT_EXPR
	      && TREE_CODE (TREE_OPERAND (curpos, 1)) == INTEGER_CST)
	    {
	      align = TREE_INT_CST_LOW (TREE_OPERAND (curpos, 1));
	      pos = compute_related_constant (curpos,
					      round_up (last_pos, align));
	    }
	  else if (!pos && TREE_CODE (curpos) == PLUS_EXPR
		   && TREE_CODE (TREE_OPERAND (curpos, 1)) == INTEGER_CST
		   && TREE_CODE (TREE_OPERAND (curpos, 0)) == MULT_EXPR
		   && host_integerp (TREE_OPERAND
				     (TREE_OPERAND (curpos, 0), 1),
				     1))
	    {
	      align
		= tree_low_cst
		(TREE_OPERAND (TREE_OPERAND (curpos, 0), 1), 1);
	      pos = compute_related_constant (curpos,
					      round_up (last_pos, align));
	    }
	  else if (potential_alignment_gap (prev_old_field, old_field,
					    pos))
	    {
	      align = TYPE_ALIGN (field_type);
	      pos = compute_related_constant (curpos,
					      round_up (last_pos, align));
	    }

	  /* If we can't compute a position, set it to zero.

	  ??? We really should abort here, but it's too much work
	  to get this correct for all cases.  */

	  if (!pos)
	    pos = bitsize_zero_node;

	  /* See if this type is variable-size and make a new type
	     and indicate the indirection if so.  */
	  if (TREE_CODE (DECL_SIZE (old_field)) != INTEGER_CST)
	    {
	      field_type = build_pointer_type (field_type);
	      var = true;
	    }

	  /* Make a new field name, if necessary.  */
	  if (var || align != 0)
	    {
	      char suffix[6];

	      if (align != 0)
		sprintf (suffix, "XV%c%u", var ? 'L' : 'A',
			 align / BITS_PER_UNIT);
	      else
		strcpy (suffix, "XVL");

	      field_name = concat_id_with_name (field_name, suffix);
	    }

	  new_field = create_field_decl (field_name, field_type,
					 new_record_type, 0,
					 DECL_SIZE (old_field), pos, 0);
	  TREE_CHAIN (new_field) = TYPE_FIELDS (new_record_type);
	  TYPE_FIELDS (new_record_type) = new_field;

	  /* If old_field is a QUAL_UNION_TYPE, take its size as being
	     zero.  The only time it's not the last field of the record
	     is when there are other components at fixed positions after
	     it (meaning there was a rep clause for every field) and we
	     want to be able to encode them.  */
	  last_pos = size_binop (PLUS_EXPR, bit_position (old_field),
				 (TREE_CODE (TREE_TYPE (old_field))
				  == QUAL_UNION_TYPE)
				 ? bitsize_zero_node
				 : DECL_SIZE (old_field));
	  prev_old_field = old_field;
	}

      TYPE_FIELDS (new_record_type)
	= nreverse (TYPE_FIELDS (new_record_type));

      rest_of_type_compilation (new_record_type, global_bindings_p ());
    }

  rest_of_type_compilation (record_type, global_bindings_p ());
}

/* Utility function of above to merge LAST_SIZE, the previous size of a record
   with FIRST_BIT and SIZE that describe a field.  SPECIAL is nonzero
   if this represents a QUAL_UNION_TYPE in which case we must look for
   COND_EXPRs and replace a value of zero with the old size.  If HAS_REP
   is nonzero, we must take the MAX of the end position of this field
   with LAST_SIZE.  In all other cases, we use FIRST_BIT plus SIZE.

   We return an expression for the size.  */

static tree
merge_sizes (tree last_size, tree first_bit, tree size, bool special,
	     bool has_rep)
{
  tree type = TREE_TYPE (last_size);
  tree new;

  if (!special || TREE_CODE (size) != COND_EXPR)
    {
      new = size_binop (PLUS_EXPR, first_bit, size);
      if (has_rep)
	new = size_binop (MAX_EXPR, last_size, new);
    }

  else
    new = fold (build3 (COND_EXPR, type, TREE_OPERAND (size, 0),
			integer_zerop (TREE_OPERAND (size, 1))
			? last_size : merge_sizes (last_size, first_bit,
						   TREE_OPERAND (size, 1),
						   1, has_rep),
			integer_zerop (TREE_OPERAND (size, 2))
			? last_size : merge_sizes (last_size, first_bit,
						   TREE_OPERAND (size, 2),
						   1, has_rep)));

  /* We don't need any NON_VALUE_EXPRs and they can confuse us (especially
     when fed through substitute_in_expr) into thinking that a constant
     size is not constant.  */
  while (TREE_CODE (new) == NON_LVALUE_EXPR)
    new = TREE_OPERAND (new, 0);

  return new;
}

/* Utility function of above to see if OP0 and OP1, both of SIZETYPE, are
   related by the addition of a constant.  Return that constant if so.  */

static tree
compute_related_constant (tree op0, tree op1)
{
  tree op0_var, op1_var;
  tree op0_con = split_plus (op0, &op0_var);
  tree op1_con = split_plus (op1, &op1_var);
  tree result = size_binop (MINUS_EXPR, op0_con, op1_con);

  if (operand_equal_p (op0_var, op1_var, 0))
    return result;
  else if (operand_equal_p (op0, size_binop (PLUS_EXPR, op1_var, result), 0))
    return result;
  else
    return 0;
}

/* Utility function of above to split a tree OP which may be a sum, into a
   constant part, which is returned, and a variable part, which is stored
   in *PVAR.  *PVAR may be bitsize_zero_node.  All operations must be of
   bitsizetype.  */

static tree
split_plus (tree in, tree *pvar)
{
  /* Strip NOPS in order to ease the tree traversal and maximize the
     potential for constant or plus/minus discovery. We need to be careful
     to always return and set *pvar to bitsizetype trees, but it's worth
     the effort.  */
  STRIP_NOPS (in);

  *pvar = convert (bitsizetype, in);

  if (TREE_CODE (in) == INTEGER_CST)
    {
      *pvar = bitsize_zero_node;
      return convert (bitsizetype, in);
    }
  else if (TREE_CODE (in) == PLUS_EXPR || TREE_CODE (in) == MINUS_EXPR)
    {
      tree lhs_var, rhs_var;
      tree lhs_con = split_plus (TREE_OPERAND (in, 0), &lhs_var);
      tree rhs_con = split_plus (TREE_OPERAND (in, 1), &rhs_var);

      if (lhs_var == TREE_OPERAND (in, 0)
	  && rhs_var == TREE_OPERAND (in, 1))
	return bitsize_zero_node;

      *pvar = size_binop (TREE_CODE (in), lhs_var, rhs_var);
      return size_binop (TREE_CODE (in), lhs_con, rhs_con);
    }
  else
    return bitsize_zero_node;
}

/* Return a FUNCTION_TYPE node. RETURN_TYPE is the type returned by the
   subprogram. If it is void_type_node, then we are dealing with a procedure,
   otherwise we are dealing with a function. PARAM_DECL_LIST is a list of
   PARM_DECL nodes that are the subprogram arguments.  CICO_LIST is the
   copy-in/copy-out list to be stored into TYPE_CICO_LIST.
   RETURNS_UNCONSTRAINED is nonzero if the function returns an unconstrained
   object.  RETURNS_BY_REF is nonzero if the function returns by reference.
   RETURNS_WITH_DSP is nonzero if the function is to return with a
   depressed stack pointer.  RETURNS_BY_TARGET_PTR is true if the function
   is to be passed (as its first parameter) the address of the place to copy
   its result.  */

tree
create_subprog_type (tree return_type, tree param_decl_list, tree cico_list,
                     bool returns_unconstrained, bool returns_by_ref,
                     bool returns_with_dsp, bool returns_by_target_ptr)
{
  /* A chain of TREE_LIST nodes whose TREE_VALUEs are the data type nodes of
     the subprogram formal parameters. This list is generated by traversing the
     input list of PARM_DECL nodes.  */
  tree param_type_list = NULL;
  tree param_decl;
  tree type;

  for (param_decl = param_decl_list; param_decl;
       param_decl = TREE_CHAIN (param_decl))
    param_type_list = tree_cons (NULL_TREE, TREE_TYPE (param_decl),
				 param_type_list);

  /* The list of the function parameter types has to be terminated by the void
     type to signal to the back-end that we are not dealing with a variable
     parameter subprogram, but that the subprogram has a fixed number of
     parameters.  */
  param_type_list = tree_cons (NULL_TREE, void_type_node, param_type_list);

  /* The list of argument types has been created in reverse
     so nreverse it.   */
  param_type_list = nreverse (param_type_list);

  type = build_function_type (return_type, param_type_list);

  /* TYPE may have been shared since GCC hashes types.  If it has a CICO_LIST
     or the new type should, make a copy of TYPE.  Likewise for
     RETURNS_UNCONSTRAINED and RETURNS_BY_REF.  */
  if (TYPE_CI_CO_LIST (type) || cico_list
      || TYPE_RETURNS_UNCONSTRAINED_P (type) != returns_unconstrained
      || TYPE_RETURNS_BY_REF_P (type) != returns_by_ref
      || TYPE_RETURNS_BY_TARGET_PTR_P (type) != returns_by_target_ptr)
    type = copy_type (type);

  TYPE_CI_CO_LIST (type) = cico_list;
  TYPE_RETURNS_UNCONSTRAINED_P (type) = returns_unconstrained;
  TYPE_RETURNS_STACK_DEPRESSED (type) = returns_with_dsp;
  TYPE_RETURNS_BY_REF_P (type) = returns_by_ref;
  TYPE_RETURNS_BY_TARGET_PTR_P (type) = returns_by_target_ptr;
  return type;
}

/* Return a copy of TYPE but safe to modify in any way.  */

tree
copy_type (tree type)
{
  tree new = copy_node (type);

  /* copy_node clears this field instead of copying it, because it is
     aliased with TREE_CHAIN.  */
  TYPE_STUB_DECL (new) = TYPE_STUB_DECL (type);

  TYPE_POINTER_TO (new) = 0;
  TYPE_REFERENCE_TO (new) = 0;
  TYPE_MAIN_VARIANT (new) = new;
  TYPE_NEXT_VARIANT (new) = 0;

  return new;
}

/* Return an INTEGER_TYPE of SIZETYPE with range MIN to MAX and whose
   TYPE_INDEX_TYPE is INDEX.  */

tree
create_index_type (tree min, tree max, tree index)
{
  /* First build a type for the desired range.  */
  tree type = build_index_2_type (min, max);

  /* If this type has the TYPE_INDEX_TYPE we want, return it.  Otherwise, if it
     doesn't have TYPE_INDEX_TYPE set, set it to INDEX.  If TYPE_INDEX_TYPE
     is set, but not to INDEX, make a copy of this type with the requested
     index type.  Note that we have no way of sharing these types, but that's
     only a small hole.  */
  if (TYPE_INDEX_TYPE (type) == index)
    return type;
  else if (TYPE_INDEX_TYPE (type))
    type = copy_type (type);

  SET_TYPE_INDEX_TYPE (type, index);
  create_type_decl (NULL_TREE, type, NULL, true, false, Empty);
  return type;
}

/* Return a TYPE_DECL node. TYPE_NAME gives the name of the type (a character
   string) and TYPE is a ..._TYPE node giving its data type.
   ARTIFICIAL_P is true if this is a declaration that was generated
   by the compiler.  DEBUG_INFO_P is true if we need to write debugging
   information about this type.  GNAT_NODE is used for the position of
   the decl.  */

tree
create_type_decl (tree type_name, tree type, struct attrib *attr_list,
		  bool artificial_p, bool debug_info_p, Node_Id gnat_node)
{
  tree type_decl = build_decl (TYPE_DECL, type_name, type);
  enum tree_code code = TREE_CODE (type);

  DECL_ARTIFICIAL (type_decl) = artificial_p;

  process_attributes (type_decl, attr_list);

  /* Pass type declaration information to the debugger unless this is an
     UNCONSTRAINED_ARRAY_TYPE, which the debugger does not support,
     and ENUMERAL_TYPE or RECORD_TYPE which is handled separately,
     a dummy type, which will be completed later, or a type for which
     debugging information was not requested.  */
  if (code == UNCONSTRAINED_ARRAY_TYPE || TYPE_IS_DUMMY_P (type)
      || !debug_info_p)
    DECL_IGNORED_P (type_decl) = 1;
  else if (code != ENUMERAL_TYPE && code != RECORD_TYPE
	   && !((code == POINTER_TYPE || code == REFERENCE_TYPE)
		&& TYPE_IS_DUMMY_P (TREE_TYPE (type))))
    rest_of_decl_compilation (type_decl, global_bindings_p (), 0);

  if (!TYPE_IS_DUMMY_P (type))
    gnat_pushdecl (type_decl, gnat_node);

  return type_decl;
}

/* Returns a GCC VAR_DECL node. VAR_NAME gives the name of the variable.
   ASM_NAME is its assembler name (if provided).  TYPE is its data type
   (a GCC ..._TYPE node).  VAR_INIT is the GCC tree for an optional initial
   expression; NULL_TREE if none.

   CONST_FLAG is true if this variable is constant.

   PUBLIC_FLAG is true if this definition is to be made visible outside of
   the current compilation unit. This flag should be set when processing the
   variable definitions in a package specification.  EXTERN_FLAG is nonzero
   when processing an external variable declaration (as opposed to a
   definition: no storage is to be allocated for the variable here).

   STATIC_FLAG is only relevant when not at top level.  In that case
   it indicates whether to always allocate storage to the variable.

   GNAT_NODE is used for the position of the decl.  */

tree
create_var_decl (tree var_name, tree asm_name, tree type, tree var_init,
                 bool const_flag, bool public_flag, bool extern_flag,
                 bool static_flag, struct attrib *attr_list, Node_Id gnat_node)
{
  bool init_const
    = (!var_init
       ? false
       : (TYPE_MAIN_VARIANT (type) == TYPE_MAIN_VARIANT (TREE_TYPE (var_init))
	  && (global_bindings_p () || static_flag
	      ? 0 != initializer_constant_valid_p (var_init,
						   TREE_TYPE (var_init))
	      : TREE_CONSTANT (var_init))));
  tree var_decl
    = build_decl ((const_flag && init_const
		   /* Only make a CONST_DECL for sufficiently-small objects.
		      We consider complex double "sufficiently-small"  */
		   && TYPE_SIZE (type) != 0
		   && host_integerp (TYPE_SIZE_UNIT (type), 1)
		   && 0 >= compare_tree_int (TYPE_SIZE_UNIT (type),
					     GET_MODE_SIZE (DCmode)))
		  ? CONST_DECL : VAR_DECL, var_name, type);

  /* If this is external, throw away any initializations unless this is a
     CONST_DECL (meaning we have a constant); they will be done elsewhere.
     If we are defining a global here, leave a constant initialization and
     save any variable elaborations for the elaboration routine.  If we are
     just annotating types, throw away the initialization if it isn't a
     constant.  */
  if ((extern_flag && TREE_CODE (var_decl) != CONST_DECL)
      || (type_annotate_only && var_init && !TREE_CONSTANT (var_init)))
    var_init = NULL_TREE;

  /* At the global level, an initializer requiring code to be generated
     produces elaboration statements.  Check that such statements are allowed,
     that is, not violating a No_Elaboration_Code restriction.  */
  if (global_bindings_p () && var_init != 0 && ! init_const)
    Check_Elaboration_Code_Allowed (gnat_node);

  /* Ada doesn't feature Fortran-like COMMON variables so we shouldn't
     try to fiddle with DECL_COMMON.  However, on platforms that don't
     support global BSS sections, uninitialized global variables would
     go in DATA instead, thus increasing the size of the executable.  */
#if !defined(ASM_OUTPUT_BSS) && !defined(ASM_OUTPUT_ALIGNED_BSS)
  if (TREE_CODE (var_decl) == VAR_DECL)
    DECL_COMMON   (var_decl) = !flag_no_common;
#endif
  DECL_INITIAL  (var_decl) = var_init;
  TREE_READONLY (var_decl) = const_flag;
  DECL_EXTERNAL (var_decl) = extern_flag;
  TREE_PUBLIC   (var_decl) = public_flag || extern_flag;
  TREE_CONSTANT (var_decl) = TREE_CODE (var_decl) == CONST_DECL;
  TREE_THIS_VOLATILE (var_decl) = TREE_SIDE_EFFECTS (var_decl)
    = TYPE_VOLATILE (type);

  /* If it's public and not external, always allocate storage for it.
     At the global binding level we need to allocate static storage for the
     variable if and only if it's not external. If we are not at the top level
     we allocate automatic storage unless requested not to.  */
  TREE_STATIC (var_decl)
    = public_flag || (global_bindings_p () ? !extern_flag : static_flag);

  if (asm_name && VAR_OR_FUNCTION_DECL_P (var_decl))
    SET_DECL_ASSEMBLER_NAME (var_decl, asm_name);

  process_attributes (var_decl, attr_list);

  /* Add this decl to the current binding level.  */
  gnat_pushdecl (var_decl, gnat_node);

  if (TREE_SIDE_EFFECTS (var_decl))
    TREE_ADDRESSABLE (var_decl) = 1;

  if (TREE_CODE (var_decl) != CONST_DECL)
    rest_of_decl_compilation (var_decl, global_bindings_p (), 0);
  else
    /* expand CONST_DECLs to set their MODE, ALIGN, SIZE and SIZE_UNIT,
       which we need for later back-annotations.  */
    expand_decl (var_decl);

  return var_decl;
}

/* Returns a FIELD_DECL node. FIELD_NAME the field name, FIELD_TYPE is its
   type, and RECORD_TYPE is the type of the parent.  PACKED is nonzero if
   this field is in a record type with a "pragma pack".  If SIZE is nonzero
   it is the specified size for this field.  If POS is nonzero, it is the bit
   position.  If ADDRESSABLE is nonzero, it means we are allowed to take
   the address of this field for aliasing purposes. If it is negative, we
   should not make a bitfield, which is used by make_aligning_type.   */

tree
create_field_decl (tree field_name, tree field_type, tree record_type,
                   int packed, tree size, tree pos, int addressable)
{
  tree field_decl = build_decl (FIELD_DECL, field_name, field_type);

  DECL_CONTEXT (field_decl) = record_type;
  TREE_READONLY (field_decl) = TYPE_READONLY (field_type);

  /* If FIELD_TYPE is BLKmode, we must ensure this is aligned to at least a
     byte boundary since GCC cannot handle less-aligned BLKmode bitfields.  */
  if (packed && TYPE_MODE (field_type) == BLKmode)
    DECL_ALIGN (field_decl) = BITS_PER_UNIT;

  /* If a size is specified, use it.  Otherwise, if the record type is packed
     compute a size to use, which may differ from the object's natural size.
     We always set a size in this case to trigger the checks for bitfield
     creation below, which is typically required when no position has been
     specified.  */
  if (size)
    size = convert (bitsizetype, size);
  else if (packed == 1)
    {
      size = rm_size (field_type);

      /* For a constant size larger than MAX_FIXED_MODE_SIZE, round up to
         byte.  */
      if (TREE_CODE (size) == INTEGER_CST
          && compare_tree_int (size, MAX_FIXED_MODE_SIZE) > 0)
        size = round_up (size, BITS_PER_UNIT);
    }

  /* If we may, according to ADDRESSABLE, make a bitfield if a size is
     specified for two reasons: first if the size differs from the natural
     size.  Second, if the alignment is insufficient.  There are a number of
     ways the latter can be true.

     We never make a bitfield if the type of the field has a nonconstant size,
     because no such entity requiring bitfield operations should reach here.

     We do *preventively* make a bitfield when there might be the need for it
     but we don't have all the necessary information to decide, as is the case
     of a field with no specified position in a packed record.

     We also don't look at STRICT_ALIGNMENT here, and rely on later processing
     in layout_decl or finish_record_type to clear the bit_field indication if
     it is in fact not needed.  */
  if (addressable >= 0
      && size
      && TREE_CODE (size) == INTEGER_CST
      && TREE_CODE (TYPE_SIZE (field_type)) == INTEGER_CST
      && (!operand_equal_p (TYPE_SIZE (field_type), size, 0)
	  || (pos && !value_factor_p (pos, TYPE_ALIGN (field_type)))
	  || packed
	  || (TYPE_ALIGN (record_type) != 0
	      && TYPE_ALIGN (record_type) < TYPE_ALIGN (field_type))))
    {
      DECL_BIT_FIELD (field_decl) = 1;
      DECL_SIZE (field_decl) = size;
      if (!packed && !pos)
	DECL_ALIGN (field_decl)
	  = (TYPE_ALIGN (record_type) != 0
	     ? MIN (TYPE_ALIGN (record_type), TYPE_ALIGN (field_type))
	     : TYPE_ALIGN (field_type));
    }

  DECL_PACKED (field_decl) = pos ? DECL_BIT_FIELD (field_decl) : packed;
  DECL_ALIGN (field_decl)
    = MAX (DECL_ALIGN (field_decl),
	   DECL_BIT_FIELD (field_decl) ? 1
	   : packed && TYPE_MODE (field_type) != BLKmode ? BITS_PER_UNIT
	   : TYPE_ALIGN (field_type));

  if (pos)
    {
      /* We need to pass in the alignment the DECL is known to have.
	 This is the lowest-order bit set in POS, but no more than
	 the alignment of the record, if one is specified.  Note
	 that an alignment of 0 is taken as infinite.  */
      unsigned int known_align;

      if (host_integerp (pos, 1))
	known_align = tree_low_cst (pos, 1) & - tree_low_cst (pos, 1);
      else
	known_align = BITS_PER_UNIT;

      if (TYPE_ALIGN (record_type)
	  && (known_align == 0 || known_align > TYPE_ALIGN (record_type)))
	known_align = TYPE_ALIGN (record_type);

      layout_decl (field_decl, known_align);
      SET_DECL_OFFSET_ALIGN (field_decl,
			     host_integerp (pos, 1) ? BIGGEST_ALIGNMENT
			     : BITS_PER_UNIT);
      pos_from_bit (&DECL_FIELD_OFFSET (field_decl),
		    &DECL_FIELD_BIT_OFFSET (field_decl),
		    DECL_OFFSET_ALIGN (field_decl), pos);

      DECL_HAS_REP_P (field_decl) = 1;
    }

  /* If the field type is passed by reference, we will have pointers to the
     field, so it is addressable. */
  if (must_pass_by_ref (field_type) || default_pass_by_ref (field_type))
    addressable = 1;

  /* ??? For now, we say that any field of aggregate type is addressable
     because the front end may take 'Reference of it.  */
  if (AGGREGATE_TYPE_P (field_type))
    addressable = 1;

  /* Mark the decl as nonaddressable if it is indicated so semantically,
     meaning we won't ever attempt to take the address of the field.

     It may also be "technically" nonaddressable, meaning that even if we
     attempt to take the field's address we will actually get the address of a
     copy. This is the case for true bitfields, but the DECL_BIT_FIELD value
     we have at this point is not accurate enough, so we don't account for
     this here and let finish_record_type decide.  */
  DECL_NONADDRESSABLE_P (field_decl) = !addressable;

  return field_decl;
}

/* Subroutine of previous function: return nonzero if EXP, ignoring any side
   effects, has the value of zero.  */

static bool
value_zerop (tree exp)
{
  if (TREE_CODE (exp) == COMPOUND_EXPR)
    return value_zerop (TREE_OPERAND (exp, 1));

  return integer_zerop (exp);
}

/* Returns a PARM_DECL node. PARAM_NAME is the name of the parameter,
   PARAM_TYPE is its type.  READONLY is true if the parameter is
   readonly (either an IN parameter or an address of a pass-by-ref
   parameter). */

tree
create_param_decl (tree param_name, tree param_type, bool readonly)
{
  tree param_decl = build_decl (PARM_DECL, param_name, param_type);

  /* Honor targetm.calls.promote_prototypes(), as not doing so can
     lead to various ABI violations.  */
  if (targetm.calls.promote_prototypes (param_type)
      && (TREE_CODE (param_type) == INTEGER_TYPE
	  || TREE_CODE (param_type) == ENUMERAL_TYPE)
      && TYPE_PRECISION (param_type) < TYPE_PRECISION (integer_type_node))
    {
      /* We have to be careful about biased types here.  Make a subtype
	 of integer_type_node with the proper biasing.  */
      if (TREE_CODE (param_type) == INTEGER_TYPE
	  && TYPE_BIASED_REPRESENTATION_P (param_type))
	{
	  param_type
	    = copy_type (build_range_type (integer_type_node,
					   TYPE_MIN_VALUE (param_type),
					   TYPE_MAX_VALUE (param_type)));

	  TYPE_BIASED_REPRESENTATION_P (param_type) = 1;
	}
      else
	param_type = integer_type_node;
    }

  DECL_ARG_TYPE (param_decl) = param_type;
  TREE_READONLY (param_decl) = readonly;
  return param_decl;
}

/* Given a DECL and ATTR_LIST, process the listed attributes.  */

void
process_attributes (tree decl, struct attrib *attr_list)
{
  for (; attr_list; attr_list = attr_list->next)
    switch (attr_list->type)
      {
      case ATTR_MACHINE_ATTRIBUTE:
	decl_attributes (&decl, tree_cons (attr_list->name, attr_list->args,
					   NULL_TREE),
			 ATTR_FLAG_TYPE_IN_PLACE);
	break;

      case ATTR_LINK_ALIAS:
        if (! DECL_EXTERNAL (decl))
	  {
	    TREE_STATIC (decl) = 1;
	    assemble_alias (decl, attr_list->name);
	  }
	break;

      case ATTR_WEAK_EXTERNAL:
	if (SUPPORTS_WEAK)
	  declare_weak (decl);
	else
	  post_error ("?weak declarations not supported on this target",
		      attr_list->error_point);
	break;

      case ATTR_LINK_SECTION:
	if (targetm.have_named_sections)
	  {
	    DECL_SECTION_NAME (decl)
	      = build_string (IDENTIFIER_LENGTH (attr_list->name),
			      IDENTIFIER_POINTER (attr_list->name));
	    DECL_COMMON (decl) = 0;
	  }
	else
	  post_error ("?section attributes are not supported for this target",
		      attr_list->error_point);
	break;

      case ATTR_LINK_CONSTRUCTOR:
	DECL_STATIC_CONSTRUCTOR (decl) = 1;
	TREE_USED (decl) = 1;
	break;

      case ATTR_LINK_DESTRUCTOR:
	DECL_STATIC_DESTRUCTOR (decl) = 1;
	TREE_USED (decl) = 1;
	break;
      }
}

/* Return true if VALUE is a known to be a multiple of FACTOR, which must be
   a power of 2. */

static bool
value_factor_p (tree value, HOST_WIDE_INT factor)
{
  if (host_integerp (value, 1))
    return tree_low_cst (value, 1) % factor == 0;

  if (TREE_CODE (value) == MULT_EXPR)
    return (value_factor_p (TREE_OPERAND (value, 0), factor)
            || value_factor_p (TREE_OPERAND (value, 1), factor));

  return 0;
}

/* Given 2 consecutive field decls PREV_FIELD and CURR_FIELD, return true
   unless we can prove these 2 fields are laid out in such a way that no gap
   exist between the end of PREV_FIELD and the beginning of CURR_FIELD.  OFFSET
   is the distance in bits between the end of PREV_FIELD and the starting
   position of CURR_FIELD. It is ignored if null. */

static bool
potential_alignment_gap (tree prev_field, tree curr_field, tree offset)
{
  /* If this is the first field of the record, there cannot be any gap */
  if (!prev_field)
    return false;

  /* If the previous field is a union type, then return False: The only
     time when such a field is not the last field of the record is when
     there are other components at fixed positions after it (meaning there
     was a rep clause for every field), in which case we don't want the
     alignment constraint to override them. */
  if (TREE_CODE (TREE_TYPE (prev_field)) == QUAL_UNION_TYPE)
    return false;

  /* If the distance between the end of prev_field and the beginning of
     curr_field is constant, then there is a gap if the value of this
     constant is not null. */
  if (offset && host_integerp (offset, 1))
    return !integer_zerop (offset);

  /* If the size and position of the previous field are constant,
     then check the sum of this size and position. There will be a gap
     iff it is not multiple of the current field alignment. */
  if (host_integerp (DECL_SIZE (prev_field), 1)
      && host_integerp (bit_position (prev_field), 1))
    return ((tree_low_cst (bit_position (prev_field), 1)
	     + tree_low_cst (DECL_SIZE (prev_field), 1))
	    % DECL_ALIGN (curr_field) != 0);

  /* If both the position and size of the previous field are multiples
     of the current field alignment, there cannot be any gap. */
  if (value_factor_p (bit_position (prev_field), DECL_ALIGN (curr_field))
      && value_factor_p (DECL_SIZE (prev_field), DECL_ALIGN (curr_field)))
    return false;

  /* Fallback, return that there may be a potential gap */
  return true;
}

/* Returns a LABEL_DECL node for LABEL_NAME.  */

tree
create_label_decl (tree label_name)
{
  tree label_decl = build_decl (LABEL_DECL, label_name, void_type_node);

  DECL_CONTEXT (label_decl)     = current_function_decl;
  DECL_MODE (label_decl)        = VOIDmode;
  DECL_SOURCE_LOCATION (label_decl) = input_location;

  return label_decl;
}

/* Returns a FUNCTION_DECL node.  SUBPROG_NAME is the name of the subprogram,
   ASM_NAME is its assembler name, SUBPROG_TYPE is its type (a FUNCTION_TYPE
   node), PARAM_DECL_LIST is the list of the subprogram arguments (a list of
   PARM_DECL nodes chained through the TREE_CHAIN field).

   INLINE_FLAG, PUBLIC_FLAG, EXTERN_FLAG, and ATTR_LIST are used to set the
   appropriate fields in the FUNCTION_DECL.  GNAT_NODE gives the location.  */

tree
create_subprog_decl (tree subprog_name, tree asm_name,
                     tree subprog_type, tree param_decl_list, bool inline_flag,
		     bool public_flag, bool extern_flag,
                     struct attrib *attr_list, Node_Id gnat_node)
{
  tree return_type  = TREE_TYPE (subprog_type);
  tree subprog_decl = build_decl (FUNCTION_DECL, subprog_name, subprog_type);

  /* If this is a function nested inside an inlined external function, it
     means we aren't going to compile the outer function unless it is
     actually inlined, so do the same for us.  */
  if (current_function_decl && DECL_INLINE (current_function_decl)
      && DECL_EXTERNAL (current_function_decl))
    extern_flag = true;

  DECL_EXTERNAL (subprog_decl)  = extern_flag;
  TREE_PUBLIC (subprog_decl)    = public_flag;
  TREE_STATIC (subprog_decl)	= 1;
  TREE_READONLY (subprog_decl)  = TYPE_READONLY (subprog_type);
  TREE_THIS_VOLATILE (subprog_decl) = TYPE_VOLATILE (subprog_type);
  TREE_SIDE_EFFECTS (subprog_decl) = TYPE_VOLATILE (subprog_type);
  DECL_ARGUMENTS (subprog_decl) = param_decl_list;
  DECL_RESULT (subprog_decl)    = build_decl (RESULT_DECL, 0, return_type);
  DECL_ARTIFICIAL (DECL_RESULT (subprog_decl)) = 1;
  DECL_IGNORED_P (DECL_RESULT (subprog_decl)) = 1;

  if (inline_flag)
    DECL_DECLARED_INLINE_P (subprog_decl) = 1;

  if (asm_name)
    SET_DECL_ASSEMBLER_NAME (subprog_decl, asm_name);

  process_attributes (subprog_decl, attr_list);

  /* Add this decl to the current binding level.  */
  gnat_pushdecl (subprog_decl, gnat_node);

  /* Output the assembler code and/or RTL for the declaration.  */
  rest_of_decl_compilation (subprog_decl, global_bindings_p (), 0);

  return subprog_decl;
}

/* Set up the framework for generating code for SUBPROG_DECL, a subprogram
   body. This routine needs to be invoked before processing the declarations
   appearing in the subprogram.  */

void
begin_subprog_body (tree subprog_decl)
{
  tree param_decl;

  current_function_decl = subprog_decl;
  announce_function (subprog_decl);

  /* Enter a new binding level and show that all the parameters belong to
     this function.  */
  gnat_pushlevel ();
  for (param_decl = DECL_ARGUMENTS (subprog_decl); param_decl;
       param_decl = TREE_CHAIN (param_decl))
    DECL_CONTEXT (param_decl) = subprog_decl;

  make_decl_rtl (subprog_decl);

  /* We handle pending sizes via the elaboration of types, so we don't need to
     save them.  This causes them to be marked as part of the outer function
     and then discarded.  */
  get_pending_sizes ();
}

/* Finish the definition of the current subprogram and compile it all the way
   to assembler language output.  BODY is the tree corresponding to
   the subprogram.  */

void
end_subprog_body (tree body)
{
  tree fndecl = current_function_decl;

  /* Mark the BLOCK for this level as being for this function and pop the
     level.  Since the vars in it are the parameters, clear them.  */
  BLOCK_VARS (current_binding_level->block) = 0;
  BLOCK_SUPERCONTEXT (current_binding_level->block) = fndecl;
  DECL_INITIAL (fndecl) = current_binding_level->block;
  gnat_poplevel ();

  /* Deal with inline.  If declared inline or we should default to inline,
     set the flag in the decl.  */
  DECL_INLINE (fndecl)
    = DECL_DECLARED_INLINE_P (fndecl) || flag_inline_trees == 2;

  /* We handle pending sizes via the elaboration of types, so we don't
     need to save them.  */
  get_pending_sizes ();

  /* Mark the RESULT_DECL as being in this subprogram. */
  DECL_CONTEXT (DECL_RESULT (fndecl)) = fndecl;

  DECL_SAVED_TREE (fndecl) = body;

  current_function_decl = DECL_CONTEXT (fndecl);
  cfun = NULL;

  /* If we're only annotating types, don't actually compile this function.  */
  if (type_annotate_only)
    return;

  /* If we don't have .ctors/.dtors sections, and this is a static
     constructor or destructor, it must be recorded now.  */
  if (DECL_STATIC_CONSTRUCTOR (fndecl) && !targetm.have_ctors_dtors)
    static_ctors = tree_cons (NULL_TREE, fndecl, static_ctors);

  if (DECL_STATIC_DESTRUCTOR (fndecl) && !targetm.have_ctors_dtors)
    static_dtors = tree_cons (NULL_TREE, fndecl, static_dtors);

  /* We do different things for nested and non-nested functions.
     ??? This should be in cgraph.  */
  if (!DECL_CONTEXT (fndecl))
    {
      gnat_gimplify_function (fndecl);
      cgraph_finalize_function (fndecl, false);
    }
  else
    /* Register this function with cgraph just far enough to get it
       added to our parent's nested function list.  */
    (void) cgraph_node (fndecl);
}

/* Convert FNDECL's code to GIMPLE and handle any nested functions.  */

static void
gnat_gimplify_function (tree fndecl)
{
  struct cgraph_node *cgn;

  dump_function (TDI_original, fndecl);
  gimplify_function_tree (fndecl);
  dump_function (TDI_generic, fndecl);

  /* Convert all nested functions to GIMPLE now.  We do things in this order
     so that items like VLA sizes are expanded properly in the context of the
     correct function.  */
  cgn = cgraph_node (fndecl);
  for (cgn = cgn->nested; cgn; cgn = cgn->next_nested)
    gnat_gimplify_function (cgn->decl);
}

/* Return a definition for a builtin function named NAME and whose data type
   is TYPE.  TYPE should be a function type with argument types.
   FUNCTION_CODE tells later passes how to compile calls to this function.
   See tree.h for its possible values.

   If LIBRARY_NAME is nonzero, use that for DECL_ASSEMBLER_NAME,
   the name to be called if we can't opencode the function.  If
   ATTRS is nonzero, use that for the function attribute list.  */

tree
builtin_function (const char *name, tree type, int function_code,
                  enum built_in_class class, const char *library_name,
                  tree attrs)
{
  tree decl = build_decl (FUNCTION_DECL, get_identifier (name), type);

  DECL_EXTERNAL (decl) = 1;
  TREE_PUBLIC (decl) = 1;
  if (library_name)
    SET_DECL_ASSEMBLER_NAME (decl, get_identifier (library_name));

  gnat_pushdecl (decl, Empty);
  DECL_BUILT_IN_CLASS (decl) = class;
  DECL_FUNCTION_CODE (decl) = function_code;
  if (attrs)
      decl_attributes (&decl, attrs, ATTR_FLAG_BUILT_IN);
  return decl;
}

/* Return an integer type with the number of bits of precision given by
   PRECISION.  UNSIGNEDP is nonzero if the type is unsigned; otherwise
   it is a signed type.  */

tree
gnat_type_for_size (unsigned precision, int unsignedp)
{
  tree t;
  char type_name[20];

  if (precision <= 2 * MAX_BITS_PER_WORD
      && signed_and_unsigned_types[precision][unsignedp])
    return signed_and_unsigned_types[precision][unsignedp];

 if (unsignedp)
    t = make_unsigned_type (precision);
  else
    t = make_signed_type (precision);

  if (precision <= 2 * MAX_BITS_PER_WORD)
    signed_and_unsigned_types[precision][unsignedp] = t;

  if (!TYPE_NAME (t))
    {
      sprintf (type_name, "%sSIGNED_%d", unsignedp ? "UN" : "", precision);
      TYPE_NAME (t) = get_identifier (type_name);
    }

  return t;
}

/* Likewise for floating-point types.  */

static tree
float_type_for_precision (int precision, enum machine_mode mode)
{
  tree t;
  char type_name[20];

  if (float_types[(int) mode])
    return float_types[(int) mode];

  float_types[(int) mode] = t = make_node (REAL_TYPE);
  TYPE_PRECISION (t) = precision;
  layout_type (t);

  gcc_assert (TYPE_MODE (t) == mode);
  if (!TYPE_NAME (t))
    {
      sprintf (type_name, "FLOAT_%d", precision);
      TYPE_NAME (t) = get_identifier (type_name);
    }

  return t;
}

/* Return a data type that has machine mode MODE.  UNSIGNEDP selects
   an unsigned type; otherwise a signed type is returned.  */

tree
gnat_type_for_mode (enum machine_mode mode, int unsignedp)
{
  if (mode == BLKmode)
    return NULL_TREE;
  else if (mode == VOIDmode)
    return void_type_node;
  else if (COMPLEX_MODE_P (mode))
    return NULL_TREE;
  else if (SCALAR_FLOAT_MODE_P (mode))
    return float_type_for_precision (GET_MODE_PRECISION (mode), mode);
  else if (SCALAR_INT_MODE_P (mode))
    return gnat_type_for_size (GET_MODE_BITSIZE (mode), unsignedp);
  else
    return NULL_TREE;
}

/* Return the unsigned version of a TYPE_NODE, a scalar type.  */

tree
gnat_unsigned_type (tree type_node)
{
  tree type = gnat_type_for_size (TYPE_PRECISION (type_node), 1);

  if (TREE_CODE (type_node) == INTEGER_TYPE && TYPE_MODULAR_P (type_node))
    {
      type = copy_node (type);
      TREE_TYPE (type) = type_node;
    }
  else if (TREE_TYPE (type_node)
	   && TREE_CODE (TREE_TYPE (type_node)) == INTEGER_TYPE
	   && TYPE_MODULAR_P (TREE_TYPE (type_node)))
    {
      type = copy_node (type);
      TREE_TYPE (type) = TREE_TYPE (type_node);
    }

  return type;
}

/* Return the signed version of a TYPE_NODE, a scalar type.  */

tree
gnat_signed_type (tree type_node)
{
  tree type = gnat_type_for_size (TYPE_PRECISION (type_node), 0);

  if (TREE_CODE (type_node) == INTEGER_TYPE && TYPE_MODULAR_P (type_node))
    {
      type = copy_node (type);
      TREE_TYPE (type) = type_node;
    }
  else if (TREE_TYPE (type_node)
	   && TREE_CODE (TREE_TYPE (type_node)) == INTEGER_TYPE
	   && TYPE_MODULAR_P (TREE_TYPE (type_node)))
    {
      type = copy_node (type);
      TREE_TYPE (type) = TREE_TYPE (type_node);
    }

  return type;
}

/* Return a type the same as TYPE except unsigned or signed according to
   UNSIGNEDP.  */

tree
gnat_signed_or_unsigned_type (int unsignedp, tree type)
{
  if (!INTEGRAL_TYPE_P (type) || TYPE_UNSIGNED (type) == unsignedp)
    return type;
  else
    return gnat_type_for_size (TYPE_PRECISION (type), unsignedp);
}

/* EXP is an expression for the size of an object.  If this size contains
   discriminant references, replace them with the maximum (if MAX_P) or
   minimum (if !MAX_P) possible value of the discriminant.  */

tree
max_size (tree exp, bool max_p)
{
  enum tree_code code = TREE_CODE (exp);
  tree type = TREE_TYPE (exp);

  switch (TREE_CODE_CLASS (code))
    {
    case tcc_declaration:
    case tcc_constant:
      return exp;

    case tcc_exceptional:
      if (code == TREE_LIST)
	return tree_cons (TREE_PURPOSE (exp),
			  max_size (TREE_VALUE (exp), max_p),
			  TREE_CHAIN (exp)
			  ? max_size (TREE_CHAIN (exp), max_p) : NULL_TREE);
      break;

    case tcc_reference:
      /* If this contains a PLACEHOLDER_EXPR, it is the thing we want to
	 modify.  Otherwise, we treat it like a variable.  */
      if (!CONTAINS_PLACEHOLDER_P (exp))
	return exp;

      type = TREE_TYPE (TREE_OPERAND (exp, 1));
      return
	max_size (max_p ? TYPE_MAX_VALUE (type) : TYPE_MIN_VALUE (type), true);

    case tcc_comparison:
      return max_p ? size_one_node : size_zero_node;

    case tcc_unary:
    case tcc_binary:
    case tcc_expression:
      switch (TREE_CODE_LENGTH (code))
	{
	case 1:
	  if (code == NON_LVALUE_EXPR)
	    return max_size (TREE_OPERAND (exp, 0), max_p);
	  else
	    return
	      fold (build1 (code, type,
			    max_size (TREE_OPERAND (exp, 0),
				      code == NEGATE_EXPR ? !max_p : max_p)));

	case 2:
	  if (code == COMPOUND_EXPR)
	    return max_size (TREE_OPERAND (exp, 1), max_p);

	  {
	    tree lhs = max_size (TREE_OPERAND (exp, 0), max_p);
	    tree rhs = max_size (TREE_OPERAND (exp, 1),
				 code == MINUS_EXPR ? !max_p : max_p);

	    /* Special-case wanting the maximum value of a MIN_EXPR.
	       In that case, if one side overflows, return the other.
	       sizetype is signed, but we know sizes are non-negative.
	       Likewise, handle a MINUS_EXPR or PLUS_EXPR with the LHS
	       overflowing or the maximum possible value and the RHS
	       a variable.  */
	    if (max_p
		&& code == MIN_EXPR
		&& TREE_CODE (rhs) == INTEGER_CST
		&& TREE_OVERFLOW (rhs))
	      return lhs;
	    else if (max_p
		     && code == MIN_EXPR
		     && TREE_CODE (lhs) == INTEGER_CST
		     && TREE_OVERFLOW (lhs))
	      return rhs;
	    else if ((code == MINUS_EXPR || code == PLUS_EXPR)
		     && ((TREE_CODE (lhs) == INTEGER_CST
			  && TREE_OVERFLOW (lhs))
			 || operand_equal_p (lhs, TYPE_MAX_VALUE (type), 0))
		     && !TREE_CONSTANT (rhs))
	      return lhs;
	    else
	      return fold (build2 (code, type, lhs, rhs));
	  }

	case 3:
	  if (code == SAVE_EXPR)
	    return exp;
	  else if (code == COND_EXPR)
	    return fold (build2 (max_p ? MAX_EXPR : MIN_EXPR, type,
				 max_size (TREE_OPERAND (exp, 1), max_p),
				 max_size (TREE_OPERAND (exp, 2), max_p)));
	  else if (code == CALL_EXPR && TREE_OPERAND (exp, 1))
	    return build3 (CALL_EXPR, type, TREE_OPERAND (exp, 0),
			   max_size (TREE_OPERAND (exp, 1), max_p), NULL);
	}

      /* Other tree classes cannot happen.  */
    default:
      break;
    }

  gcc_unreachable ();
}

/* Build a template of type TEMPLATE_TYPE from the array bounds of ARRAY_TYPE.
   EXPR is an expression that we can use to locate any PLACEHOLDER_EXPRs.
   Return a constructor for the template.  */

tree
build_template (tree template_type, tree array_type, tree expr)
{
  tree template_elts = NULL_TREE;
  tree bound_list = NULL_TREE;
  tree field;

  if (TREE_CODE (array_type) == RECORD_TYPE
      && (TYPE_IS_PADDING_P (array_type)
	  || TYPE_JUSTIFIED_MODULAR_P (array_type)))
    array_type = TREE_TYPE (TYPE_FIELDS (array_type));

  if (TREE_CODE (array_type) == ARRAY_TYPE
      || (TREE_CODE (array_type) == INTEGER_TYPE
	  && TYPE_HAS_ACTUAL_BOUNDS_P (array_type)))
    bound_list = TYPE_ACTUAL_BOUNDS (array_type);

  /* First make the list for a CONSTRUCTOR for the template.   Go down the
     field list of the template instead of the type chain because this
     array might be an Ada array of arrays and we can't tell where the
     nested arrays stop being the underlying object.  */

  for (field = TYPE_FIELDS (template_type); field;
       (bound_list
	? (bound_list = TREE_CHAIN (bound_list))
	: (array_type = TREE_TYPE (array_type))),
       field = TREE_CHAIN (TREE_CHAIN (field)))
    {
      tree bounds, min, max;

      /* If we have a bound list, get the bounds from there.  Likewise
	 for an ARRAY_TYPE.  Otherwise, if expr is a PARM_DECL with
	 DECL_BY_COMPONENT_PTR_P, use the bounds of the field in the template.
	 This will give us a maximum range.  */
      if (bound_list)
	bounds = TREE_VALUE (bound_list);
      else if (TREE_CODE (array_type) == ARRAY_TYPE)
	bounds = TYPE_INDEX_TYPE (TYPE_DOMAIN (array_type));
      else if (expr && TREE_CODE (expr) == PARM_DECL
	       && DECL_BY_COMPONENT_PTR_P (expr))
	bounds = TREE_TYPE (field);
      else
	gcc_unreachable ();

      min = convert (TREE_TYPE (TREE_CHAIN (field)), TYPE_MIN_VALUE (bounds));
      max = convert (TREE_TYPE (field), TYPE_MAX_VALUE (bounds));

      /* If either MIN or MAX involve a PLACEHOLDER_EXPR, we must
	 substitute it from OBJECT.  */
      min = SUBSTITUTE_PLACEHOLDER_IN_EXPR (min, expr);
      max = SUBSTITUTE_PLACEHOLDER_IN_EXPR (max, expr);

      template_elts = tree_cons (TREE_CHAIN (field), max,
				 tree_cons (field, min, template_elts));
    }

  return gnat_build_constructor (template_type, nreverse (template_elts));
}

/* Build a VMS descriptor from a Mechanism_Type, which must specify
   a descriptor type, and the GCC type of an object.  Each FIELD_DECL
   in the type contains in its DECL_INITIAL the expression to use when
   a constructor is made for the type.  GNAT_ENTITY is an entity used
   to print out an error message if the mechanism cannot be applied to
   an object of that type and also for the name.  */

tree
build_vms_descriptor (tree type, Mechanism_Type mech, Entity_Id gnat_entity)
{
  tree record_type = make_node (RECORD_TYPE);
  tree field_list = 0;
  int class;
  int dtype = 0;
  tree inner_type;
  int ndim;
  int i;
  tree *idx_arr;
  tree tem;

  /* If TYPE is an unconstrained array, use the underlying array type.  */
  if (TREE_CODE (type) == UNCONSTRAINED_ARRAY_TYPE)
    type = TREE_TYPE (TREE_TYPE (TYPE_FIELDS (TREE_TYPE (type))));

  /* If this is an array, compute the number of dimensions in the array,
     get the index types, and point to the inner type.  */
  if (TREE_CODE (type) != ARRAY_TYPE)
    ndim = 0;
  else
    for (ndim = 1, inner_type = type;
	 TREE_CODE (TREE_TYPE (inner_type)) == ARRAY_TYPE
	 && TYPE_MULTI_ARRAY_P (TREE_TYPE (inner_type));
	 ndim++, inner_type = TREE_TYPE (inner_type))
      ;

  idx_arr = (tree *) alloca (ndim * sizeof (tree));

  if (mech != By_Descriptor_NCA
      && TREE_CODE (type) == ARRAY_TYPE && TYPE_CONVENTION_FORTRAN_P (type))
    for (i = ndim - 1, inner_type = type;
	 i >= 0;
	 i--, inner_type = TREE_TYPE (inner_type))
      idx_arr[i] = TYPE_DOMAIN (inner_type);
  else
    for (i = 0, inner_type = type;
	 i < ndim;
	 i++, inner_type = TREE_TYPE (inner_type))
      idx_arr[i] = TYPE_DOMAIN (inner_type);

  /* Now get the DTYPE value.  */
  switch (TREE_CODE (type))
    {
    case INTEGER_TYPE:
    case ENUMERAL_TYPE:
      if (TYPE_VAX_FLOATING_POINT_P (type))
	switch (tree_low_cst (TYPE_DIGITS_VALUE (type), 1))
	  {
	  case 6:
	    dtype = 10;
	    break;
	  case 9:
	    dtype = 11;
	    break;
	  case 15:
	    dtype = 27;
	    break;
	  }
      else
	switch (GET_MODE_BITSIZE (TYPE_MODE (type)))
	  {
	  case 8:
	    dtype = TYPE_UNSIGNED (type) ? 2 : 6;
	    break;
	  case 16:
	    dtype = TYPE_UNSIGNED (type) ? 3 : 7;
	    break;
	  case 32:
	    dtype = TYPE_UNSIGNED (type) ? 4 : 8;
	    break;
	  case 64:
	    dtype = TYPE_UNSIGNED (type) ? 5 : 9;
	    break;
	  case 128:
	    dtype = TYPE_UNSIGNED (type) ? 25 : 26;
	    break;
	  }
      break;

    case REAL_TYPE:
      dtype = GET_MODE_BITSIZE (TYPE_MODE (type)) == 32 ? 52 : 53;
      break;

    case COMPLEX_TYPE:
      if (TREE_CODE (TREE_TYPE (type)) == INTEGER_TYPE
	  && TYPE_VAX_FLOATING_POINT_P (type))
	switch (tree_low_cst (TYPE_DIGITS_VALUE (type), 1))
	  {
	  case 6:
	    dtype = 12;
	    break;
	  case 9:
	    dtype = 13;
	    break;
	  case 15:
	    dtype = 29;
	  }
      else
	dtype = GET_MODE_BITSIZE (TYPE_MODE (TREE_TYPE (type))) == 32 ? 54: 55;
      break;

    case ARRAY_TYPE:
      dtype = 14;
      break;

    default:
      break;
    }

  /* Get the CLASS value.  */
  switch (mech)
    {
    case By_Descriptor_A:
      class = 4;
      break;
    case By_Descriptor_NCA:
      class = 10;
      break;
    case By_Descriptor_SB:
      class = 15;
      break;
    default:
      class = 1;
    }

  /* Make the type for a descriptor for VMS.  The first four fields
     are the same for all types.  */

  field_list
    = chainon (field_list,
	       make_descriptor_field
	       ("LENGTH", gnat_type_for_size (16, 1), record_type,
		size_in_bytes (mech == By_Descriptor_A ? inner_type : type)));

  field_list = chainon (field_list,
			make_descriptor_field ("DTYPE",
					       gnat_type_for_size (8, 1),
					       record_type, size_int (dtype)));
  field_list = chainon (field_list,
			make_descriptor_field ("CLASS",
					       gnat_type_for_size (8, 1),
					       record_type, size_int (class)));

  field_list
    = chainon (field_list,
	       make_descriptor_field
	       ("POINTER",
		build_pointer_type_for_mode (type, SImode, false), record_type,
		build1 (ADDR_EXPR,
			build_pointer_type_for_mode (type, SImode, false),
			build0 (PLACEHOLDER_EXPR, type))));

  switch (mech)
    {
    case By_Descriptor:
    case By_Descriptor_S:
      break;

    case By_Descriptor_SB:
      field_list
	= chainon (field_list,
		   make_descriptor_field
		   ("SB_L1", gnat_type_for_size (32, 1), record_type,
		    TREE_CODE (type) == ARRAY_TYPE
		    ? TYPE_MIN_VALUE (TYPE_DOMAIN (type)) : size_zero_node));
      field_list
	= chainon (field_list,
		   make_descriptor_field
		   ("SB_L2", gnat_type_for_size (32, 1), record_type,
		    TREE_CODE (type) == ARRAY_TYPE
		    ? TYPE_MAX_VALUE (TYPE_DOMAIN (type)) : size_zero_node));
      break;

    case By_Descriptor_A:
    case By_Descriptor_NCA:
      field_list = chainon (field_list,
			    make_descriptor_field ("SCALE",
						   gnat_type_for_size (8, 1),
						   record_type,
						   size_zero_node));

      field_list = chainon (field_list,
			    make_descriptor_field ("DIGITS",
						   gnat_type_for_size (8, 1),
						   record_type,
						   size_zero_node));

      field_list
	= chainon (field_list,
		   make_descriptor_field
		   ("AFLAGS", gnat_type_for_size (8, 1), record_type,
		    size_int (mech == By_Descriptor_NCA
			      ? 0
			      /* Set FL_COLUMN, FL_COEFF, and FL_BOUNDS.  */
			      : (TREE_CODE (type) == ARRAY_TYPE
				 && TYPE_CONVENTION_FORTRAN_P (type)
				 ? 224 : 192))));

      field_list = chainon (field_list,
			    make_descriptor_field ("DIMCT",
						   gnat_type_for_size (8, 1),
						   record_type,
						   size_int (ndim)));

      field_list = chainon (field_list,
			    make_descriptor_field ("ARSIZE",
						   gnat_type_for_size (32, 1),
						   record_type,
						   size_in_bytes (type)));

      /* Now build a pointer to the 0,0,0... element.  */
      tem = build0 (PLACEHOLDER_EXPR, type);
      for (i = 0, inner_type = type; i < ndim;
	   i++, inner_type = TREE_TYPE (inner_type))
	tem = build4 (ARRAY_REF, TREE_TYPE (inner_type), tem,
		      convert (TYPE_DOMAIN (inner_type), size_zero_node),
		      NULL_TREE, NULL_TREE);

      field_list
	= chainon (field_list,
		   make_descriptor_field
		   ("A0",
		    build_pointer_type_for_mode (inner_type, SImode, false),
		    record_type,
		    build1 (ADDR_EXPR,
			    build_pointer_type_for_mode (inner_type, SImode,
							 false),
			    tem)));

      /* Next come the addressing coefficients.  */
      tem = size_int (1);
      for (i = 0; i < ndim; i++)
	{
	  char fname[3];
	  tree idx_length
	    = size_binop (MULT_EXPR, tem,
			  size_binop (PLUS_EXPR,
				      size_binop (MINUS_EXPR,
						  TYPE_MAX_VALUE (idx_arr[i]),
						  TYPE_MIN_VALUE (idx_arr[i])),
				      size_int (1)));

	  fname[0] = (mech == By_Descriptor_NCA ? 'S' : 'M');
	  fname[1] = '0' + i, fname[2] = 0;
	  field_list
	    = chainon (field_list,
		       make_descriptor_field (fname,
					      gnat_type_for_size (32, 1),
					      record_type, idx_length));

	  if (mech == By_Descriptor_NCA)
	    tem = idx_length;
	}

      /* Finally here are the bounds.  */
      for (i = 0; i < ndim; i++)
	{
	  char fname[3];

	  fname[0] = 'L', fname[1] = '0' + i, fname[2] = 0;
	  field_list
	    = chainon (field_list,
		       make_descriptor_field
		       (fname, gnat_type_for_size (32, 1), record_type,
			TYPE_MIN_VALUE (idx_arr[i])));

	  fname[0] = 'U';
	  field_list
	    = chainon (field_list,
		       make_descriptor_field
		       (fname, gnat_type_for_size (32, 1), record_type,
			TYPE_MAX_VALUE (idx_arr[i])));
	}
      break;

    default:
      post_error ("unsupported descriptor type for &", gnat_entity);
    }

  finish_record_type (record_type, field_list, false, true);
  create_type_decl (create_concat_name (gnat_entity, "DESC"), record_type,
		    NULL, true, false, gnat_entity);

  return record_type;
}

/* Utility routine for above code to make a field.  */

static tree
make_descriptor_field (const char *name, tree type,
		       tree rec_type, tree initial)
{
  tree field
    = create_field_decl (get_identifier (name), type, rec_type, 0, 0, 0, 0);

  DECL_INITIAL (field) = initial;
  return field;
}

/* Build a type to be used to represent an aliased object whose nominal
   type is an unconstrained array.  This consists of a RECORD_TYPE containing
   a field of TEMPLATE_TYPE and a field of OBJECT_TYPE, which is an
   ARRAY_TYPE.  If ARRAY_TYPE is that of the unconstrained array, this
   is used to represent an arbitrary unconstrained object.  Use NAME
   as the name of the record.  */

tree
build_unc_object_type (tree template_type, tree object_type, tree name)
{
  tree type = make_node (RECORD_TYPE);
  tree template_field = create_field_decl (get_identifier ("BOUNDS"),
					   template_type, type, 0, 0, 0, 1);
  tree array_field = create_field_decl (get_identifier ("ARRAY"), object_type,
					type, 0, 0, 0, 1);

  TYPE_NAME (type) = name;
  TYPE_CONTAINS_TEMPLATE_P (type) = 1;
  finish_record_type (type,
		      chainon (chainon (NULL_TREE, template_field),
			       array_field),
		      false, false);

  return type;
}

/* Same, taking a thin or fat pointer type instead of a template type. */

tree
build_unc_object_type_from_ptr (tree thin_fat_ptr_type, tree object_type, tree name)
{
  tree template_type;

  gcc_assert (TYPE_FAT_OR_THIN_POINTER_P (thin_fat_ptr_type));

  template_type
    = (TYPE_FAT_POINTER_P (thin_fat_ptr_type)
       ? TREE_TYPE (TREE_TYPE (TREE_CHAIN (TYPE_FIELDS (thin_fat_ptr_type))))
       : TREE_TYPE (TYPE_FIELDS (TREE_TYPE (thin_fat_ptr_type))));
  return build_unc_object_type (template_type, object_type, name);
}

/* Update anything previously pointing to OLD_TYPE to point to NEW_TYPE.  In
   the normal case this is just two adjustments, but we have more to do
   if NEW is an UNCONSTRAINED_ARRAY_TYPE.  */

void
update_pointer_to (tree old_type, tree new_type)
{
  tree ptr = TYPE_POINTER_TO (old_type);
  tree ref = TYPE_REFERENCE_TO (old_type);
  tree ptr1, ref1;
  tree type;

  /* If this is the main variant, process all the other variants first.  */
  if (TYPE_MAIN_VARIANT (old_type) == old_type)
    for (type = TYPE_NEXT_VARIANT (old_type); type;
	 type = TYPE_NEXT_VARIANT (type))
      update_pointer_to (type, new_type);

  /* If no pointer or reference, we are done.  */
  if (!ptr && !ref)
    return;

  /* Merge the old type qualifiers in the new type.

     Each old variant has qualifiers for specific reasons, and the new
     designated type as well. Each set of qualifiers represents useful
     information grabbed at some point, and merging the two simply unifies
     these inputs into the final type description.

     Consider for instance a volatile type frozen after an access to constant
     type designating it. After the designated type freeze, we get here with a
     volatile new_type and a dummy old_type with a readonly variant, created
     when the access type was processed. We shall make a volatile and readonly
     designated type, because that's what it really is.

     We might also get here for a non-dummy old_type variant with different
     qualifiers than the new_type ones, for instance in some cases of pointers
     to private record type elaboration (see the comments around the call to
     this routine from gnat_to_gnu_entity/E_Access_Type). We have to merge the
     qualifiers in thoses cases too, to avoid accidentally discarding the
     initial set, and will often end up with old_type == new_type then.  */
  new_type = build_qualified_type (new_type,
				   TYPE_QUALS (old_type)
				   | TYPE_QUALS (new_type));

  /* If the new type and the old one are identical, there is nothing to
     update.  */
  if (old_type == new_type)
    return;

  /* Otherwise, first handle the simple case.  */
  if (TREE_CODE (new_type) != UNCONSTRAINED_ARRAY_TYPE)
    {
      TYPE_POINTER_TO (new_type) = ptr;
      TYPE_REFERENCE_TO (new_type) = ref;

      for (; ptr; ptr = TYPE_NEXT_PTR_TO (ptr))
	for (ptr1 = TYPE_MAIN_VARIANT (ptr); ptr1;
	     ptr1 = TYPE_NEXT_VARIANT (ptr1))
	  TREE_TYPE (ptr1) = new_type;

      for (; ref; ref = TYPE_NEXT_REF_TO (ref))
	for (ref1 = TYPE_MAIN_VARIANT (ref); ref1;
	     ref1 = TYPE_NEXT_VARIANT (ref1))
	  TREE_TYPE (ref1) = new_type;
    }

  /* Now deal with the unconstrained array case. In this case the "pointer"
     is actually a RECORD_TYPE where the types of both fields are
     pointers to void.  In that case, copy the field list from the
     old type to the new one and update the fields' context. */
  else if (TREE_CODE (ptr) != RECORD_TYPE || !TYPE_IS_FAT_POINTER_P (ptr))
    gcc_unreachable ();

  else
    {
      tree new_obj_rec = TYPE_OBJECT_RECORD_TYPE (new_type);
      tree ptr_temp_type;
      tree new_ref;
      tree var;

      SET_DECL_ORIGINAL_FIELD (TYPE_FIELDS (ptr),
			       TYPE_FIELDS (TYPE_POINTER_TO (new_type)));
      SET_DECL_ORIGINAL_FIELD (TREE_CHAIN (TYPE_FIELDS (ptr)),
			       TREE_CHAIN (TYPE_FIELDS
					   (TYPE_POINTER_TO (new_type))));

      TYPE_FIELDS (ptr) = TYPE_FIELDS (TYPE_POINTER_TO (new_type));
      DECL_CONTEXT (TYPE_FIELDS (ptr)) = ptr;
      DECL_CONTEXT (TREE_CHAIN (TYPE_FIELDS (ptr))) = ptr;

      /* Rework the PLACEHOLDER_EXPR inside the reference to the
	 template bounds.

	 ??? This is now the only use of gnat_substitute_in_type, which
	 is now a very "heavy" routine to do this, so it should be replaced
	 at some point.  */
      ptr_temp_type = TREE_TYPE (TREE_CHAIN (TYPE_FIELDS (ptr)));
      new_ref = build3 (COMPONENT_REF, ptr_temp_type,
			build0 (PLACEHOLDER_EXPR, ptr),
			TREE_CHAIN (TYPE_FIELDS (ptr)), NULL_TREE);

      update_pointer_to
	(TREE_TYPE (TREE_TYPE (TYPE_FIELDS (ptr))),
	 gnat_substitute_in_type (TREE_TYPE (TREE_TYPE (TYPE_FIELDS (ptr))),
				  TREE_CHAIN (TYPE_FIELDS (ptr)), new_ref));

      for (var = TYPE_MAIN_VARIANT (ptr); var; var = TYPE_NEXT_VARIANT (var))
	SET_TYPE_UNCONSTRAINED_ARRAY (var, new_type);

      TYPE_POINTER_TO (new_type) = TYPE_REFERENCE_TO (new_type)
	= TREE_TYPE (new_type) = ptr;

      /* Now handle updating the allocation record, what the thin pointer
	 points to.  Update all pointers from the old record into the new
	 one, update the types of the fields, and recompute the size.  */

      update_pointer_to (TYPE_OBJECT_RECORD_TYPE (old_type), new_obj_rec);

      TREE_TYPE (TYPE_FIELDS (new_obj_rec)) = TREE_TYPE (ptr_temp_type);
      TREE_TYPE (TREE_CHAIN (TYPE_FIELDS (new_obj_rec)))
	= TREE_TYPE (TREE_TYPE (TYPE_FIELDS (ptr)));
      DECL_SIZE (TREE_CHAIN (TYPE_FIELDS (new_obj_rec)))
	= TYPE_SIZE (TREE_TYPE (TREE_TYPE (TYPE_FIELDS (ptr))));
      DECL_SIZE_UNIT (TREE_CHAIN (TYPE_FIELDS (new_obj_rec)))
	= TYPE_SIZE_UNIT (TREE_TYPE (TREE_TYPE (TYPE_FIELDS (ptr))));

      TYPE_SIZE (new_obj_rec)
	= size_binop (PLUS_EXPR,
		      DECL_SIZE (TYPE_FIELDS (new_obj_rec)),
		      DECL_SIZE (TREE_CHAIN (TYPE_FIELDS (new_obj_rec))));
      TYPE_SIZE_UNIT (new_obj_rec)
	= size_binop (PLUS_EXPR,
		      DECL_SIZE_UNIT (TYPE_FIELDS (new_obj_rec)),
		      DECL_SIZE_UNIT (TREE_CHAIN (TYPE_FIELDS (new_obj_rec))));
      rest_of_type_compilation (ptr, global_bindings_p ());
    }
}

/* Convert a pointer to a constrained array into a pointer to a fat
   pointer.  This involves making or finding a template.  */

static tree
convert_to_fat_pointer (tree type, tree expr)
{
  tree template_type = TREE_TYPE (TREE_TYPE (TREE_CHAIN (TYPE_FIELDS (type))));
  tree template, template_addr;
  tree etype = TREE_TYPE (expr);

  /* If EXPR is a constant of zero, we make a fat pointer that has a null
     pointer to the template and array.  */
  if (integer_zerop (expr))
    return
      gnat_build_constructor
	(type,
	 tree_cons (TYPE_FIELDS (type),
		    convert (TREE_TYPE (TYPE_FIELDS (type)), expr),
		    tree_cons (TREE_CHAIN (TYPE_FIELDS (type)),
			       convert (build_pointer_type (template_type),
					expr),
			       NULL_TREE)));

  /* If EXPR is a thin pointer, make the template and data from the record.  */

  else if (TYPE_THIN_POINTER_P (etype))
    {
      tree fields = TYPE_FIELDS (TREE_TYPE (etype));

      expr = save_expr (expr);
      if (TREE_CODE (expr) == ADDR_EXPR)
	expr = TREE_OPERAND (expr, 0);
      else
	expr = build1 (INDIRECT_REF, TREE_TYPE (etype), expr);

      template = build_component_ref (expr, NULL_TREE, fields, false);
      expr = build_unary_op (ADDR_EXPR, NULL_TREE,
			     build_component_ref (expr, NULL_TREE,
						  TREE_CHAIN (fields), false));
    }
  else
    /* Otherwise, build the constructor for the template.  */
    template = build_template (template_type, TREE_TYPE (etype), expr);

  template_addr = build_unary_op (ADDR_EXPR, NULL_TREE, template);

  /* The result is a CONSTRUCTOR for the fat pointer.

     If expr is an argument of a foreign convention subprogram, the type it
     points to is directly the component type. In this case, the expression
     type may not match the corresponding FIELD_DECL type at this point, so we
     call "convert" here to fix that up if necessary. This type consistency is
     required, for instance because it ensures that possible later folding of
     component_refs against this constructor always yields something of the
     same type as the initial reference.

     Note that the call to "build_template" above is still fine, because it
     will only refer to the provided template_type in this case.  */
   return
     gnat_build_constructor
     (type, tree_cons (TYPE_FIELDS (type),
 		      convert (TREE_TYPE (TYPE_FIELDS (type)), expr),
 		      tree_cons (TREE_CHAIN (TYPE_FIELDS (type)),
 				 template_addr, NULL_TREE)));
}

/* Convert to a thin pointer type, TYPE.  The only thing we know how to convert
   is something that is a fat pointer, so convert to it first if it EXPR
   is not already a fat pointer.  */

static tree
convert_to_thin_pointer (tree type, tree expr)
{
  if (!TYPE_FAT_POINTER_P (TREE_TYPE (expr)))
    expr
      = convert_to_fat_pointer
	(TREE_TYPE (TYPE_UNCONSTRAINED_ARRAY (TREE_TYPE (type))), expr);

  /* We get the pointer to the data and use a NOP_EXPR to make it the
     proper GCC type.  */
  expr = build_component_ref (expr, NULL_TREE, TYPE_FIELDS (TREE_TYPE (expr)),
			      false);
  expr = build1 (NOP_EXPR, type, expr);

  return expr;
}

/* Create an expression whose value is that of EXPR,
   converted to type TYPE.  The TREE_TYPE of the value
   is always TYPE.  This function implements all reasonable
   conversions; callers should filter out those that are
   not permitted by the language being compiled.  */

tree
convert (tree type, tree expr)
{
  enum tree_code code = TREE_CODE (type);
  tree etype = TREE_TYPE (expr);
  enum tree_code ecode = TREE_CODE (etype);
  tree tem;

  /* If EXPR is already the right type, we are done.  */
  if (type == etype)
    return expr;

  /* If the input type has padding, remove it by doing a component reference
     to the field.  If the output type has padding, make a constructor
     to build the record.  If both input and output have padding and are
     of variable size, do this as an unchecked conversion.  */
  else if (ecode == RECORD_TYPE && code == RECORD_TYPE
      && TYPE_IS_PADDING_P (type) && TYPE_IS_PADDING_P (etype)
      && (!TREE_CONSTANT (TYPE_SIZE (type))
	  || !TREE_CONSTANT (TYPE_SIZE (etype))))
    ;
  else if (ecode == RECORD_TYPE && TYPE_IS_PADDING_P (etype))
    {
      /* If we have just converted to this padded type, just get
	 the inner expression.  */
      if (TREE_CODE (expr) == CONSTRUCTOR
	  && !VEC_empty (constructor_elt, CONSTRUCTOR_ELTS (expr))
	  && VEC_index (constructor_elt, CONSTRUCTOR_ELTS (expr), 0)->index
	     == TYPE_FIELDS (etype))
	return VEC_index (constructor_elt, CONSTRUCTOR_ELTS (expr), 0)->value;
      else
	return convert (type,
			build_component_ref (expr, NULL_TREE,
					     TYPE_FIELDS (etype), false));
    }
  else if (code == RECORD_TYPE && TYPE_IS_PADDING_P (type))
    {
      /* If we previously converted from another type and our type is
	 of variable size, remove the conversion to avoid the need for
	 variable-size temporaries.  */
      if (TREE_CODE (expr) == VIEW_CONVERT_EXPR
	  && !TREE_CONSTANT (TYPE_SIZE (type)))
	expr = TREE_OPERAND (expr, 0);

      /* If we are just removing the padding from expr, convert the original
	 object if we have variable size.  That will avoid the need
	 for some variable-size temporaries.  */
      if (TREE_CODE (expr) == COMPONENT_REF
	  && TREE_CODE (TREE_TYPE (TREE_OPERAND (expr, 0))) == RECORD_TYPE
	  && TYPE_IS_PADDING_P (TREE_TYPE (TREE_OPERAND (expr, 0)))
	  && !TREE_CONSTANT (TYPE_SIZE (type)))
	return convert (type, TREE_OPERAND (expr, 0));

      /* If the result type is a padded type with a self-referentially-sized
	 field and the expression type is a record, do this as an
	 unchecked conversion.  */
      else if (TREE_CODE (etype) == RECORD_TYPE
	       && CONTAINS_PLACEHOLDER_P (DECL_SIZE (TYPE_FIELDS (type))))
	return unchecked_convert (type, expr, false);

      else
	return
	  gnat_build_constructor (type,
			     tree_cons (TYPE_FIELDS (type),
					convert (TREE_TYPE
						 (TYPE_FIELDS (type)),
						 expr),
					NULL_TREE));
    }

  /* If the input is a biased type, adjust first.  */
  if (ecode == INTEGER_TYPE && TYPE_BIASED_REPRESENTATION_P (etype))
    return convert (type, fold (build2 (PLUS_EXPR, TREE_TYPE (etype),
					fold (build1 (NOP_EXPR,
						      TREE_TYPE (etype),
						      expr)),
					TYPE_MIN_VALUE (etype))));

  /* If the input is a justified modular type, we need to extract the actual
     object before converting it to any other type with the exceptions of an
     unconstrained array or of a mere type variant.  It is useful to avoid the
     extraction and conversion in the type variant case because it could end
     up replacing a VAR_DECL expr by a constructor and we might be about the
     take the address of the result.  */
  if (ecode == RECORD_TYPE && TYPE_JUSTIFIED_MODULAR_P (etype)
      && code != UNCONSTRAINED_ARRAY_TYPE
      && TYPE_MAIN_VARIANT (type) != TYPE_MAIN_VARIANT (etype))
    return convert (type, build_component_ref (expr, NULL_TREE,
					       TYPE_FIELDS (etype), false));

  /* If converting to a type that contains a template, convert to the data
     type and then build the template. */
  if (code == RECORD_TYPE && TYPE_CONTAINS_TEMPLATE_P (type))
    {
      tree obj_type = TREE_TYPE (TREE_CHAIN (TYPE_FIELDS (type)));

      /* If the source already has a template, get a reference to the
	 associated array only, as we are going to rebuild a template
	 for the target type anyway.  */
      expr = maybe_unconstrained_array (expr);

      return
	gnat_build_constructor
	  (type,
	   tree_cons (TYPE_FIELDS (type),
		      build_template (TREE_TYPE (TYPE_FIELDS (type)),
				      obj_type, NULL_TREE),
		      tree_cons (TREE_CHAIN (TYPE_FIELDS (type)),
				 convert (obj_type, expr), NULL_TREE)));
    }

  /* There are some special cases of expressions that we process
     specially.  */
  switch (TREE_CODE (expr))
    {
    case ERROR_MARK:
      return expr;

    case NULL_EXPR:
      /* Just set its type here.  For TRANSFORM_EXPR, we will do the actual
	 conversion in gnat_expand_expr.  NULL_EXPR does not represent
	 and actual value, so no conversion is needed.  */
      expr = copy_node (expr);
      TREE_TYPE (expr) = type;
      return expr;

    case STRING_CST:
      /* If we are converting a STRING_CST to another constrained array type,
	 just make a new one in the proper type.  */
      if (code == ecode && AGGREGATE_TYPE_P (etype)
	  && !(TREE_CODE (TYPE_SIZE (etype)) == INTEGER_CST
	       && TREE_CODE (TYPE_SIZE (type)) != INTEGER_CST))
	{
	  expr = copy_node (expr);
	  TREE_TYPE (expr) = type;
	  return expr;
	}
      break;

    case UNCONSTRAINED_ARRAY_REF:
      /* Convert this to the type of the inner array by getting the address of
	 the array from the template.  */
      expr = build_unary_op (INDIRECT_REF, NULL_TREE,
			     build_component_ref (TREE_OPERAND (expr, 0),
						  get_identifier ("P_ARRAY"),
						  NULL_TREE, false));
      etype = TREE_TYPE (expr);
      ecode = TREE_CODE (etype);
      break;

    case VIEW_CONVERT_EXPR:
      {
	/* GCC 4.x is very sensitive to type consistency overall, and view
	   conversions thus are very frequent.  Eventhough just "convert"ing
	   the inner operand to the output type is fine in most cases, it
	   might expose unexpected input/output type mismatches in special
	   circumstances so we avoid such recursive calls when we can.  */

	tree op0 = TREE_OPERAND (expr, 0);

	/* If we are converting back to the original type, we can just
	   lift the input conversion.  This is a common occurence with
	   switches back-and-forth amongst type variants.  */
	if (type == TREE_TYPE (op0))
	  return op0;

	/* Otherwise, if we're converting between two aggregate types, we
	   might be allowed to substitute the VIEW_CONVERT target type in
	   place or to just convert the inner expression.  */
	if (AGGREGATE_TYPE_P (type) && AGGREGATE_TYPE_P (etype))
	  {
	    /* If we are converting between type variants, we can just
	       substitute the VIEW_CONVERT in place.  */
	    if (TYPE_MAIN_VARIANT (type) == TYPE_MAIN_VARIANT (etype))
	      return build1 (VIEW_CONVERT_EXPR, type, op0);

	    /* Otherwise, we may just bypass the input view conversion unless
	       one of the types is a fat pointer, or we're converting to an
	       unchecked union type.  Both are handled by specialized code
	       below and the latter relies on exact type matching.  */
	    else if (!TYPE_FAT_POINTER_P (type) && !TYPE_FAT_POINTER_P (etype)
		     && !(code == UNION_TYPE && TYPE_UNCHECKED_UNION_P (type)))
	      return convert (type, op0);
	  }
      }
      break;

    case INDIRECT_REF:
      /* If both types are record types, just convert the pointer and
	 make a new INDIRECT_REF.

	 ??? Disable this for now since it causes problems with the
	 code in build_binary_op for MODIFY_EXPR which wants to
	 strip off conversions.  But that code really is a mess and
	 we need to do this a much better way some time.  */
      if (0
	  && (TREE_CODE (type) == RECORD_TYPE
	      || TREE_CODE (type) == UNION_TYPE)
	  && (TREE_CODE (etype) == RECORD_TYPE
	      || TREE_CODE (etype) == UNION_TYPE)
	  && !TYPE_FAT_POINTER_P (type) && !TYPE_FAT_POINTER_P (etype))
	return build_unary_op (INDIRECT_REF, NULL_TREE,
			       convert (build_pointer_type (type),
					TREE_OPERAND (expr, 0)));
      break;

    default:
      break;
    }

  /* Check for converting to a pointer to an unconstrained array.  */
  if (TYPE_FAT_POINTER_P (type) && !TYPE_FAT_POINTER_P (etype))
    return convert_to_fat_pointer (type, expr);

  /* If we're converting between two aggregate types that have the same main
     variant, just make a VIEW_CONVER_EXPR.  */
  else if (AGGREGATE_TYPE_P (type)
	   && TYPE_MAIN_VARIANT (type) == TYPE_MAIN_VARIANT (etype))
    return build1 (VIEW_CONVERT_EXPR, type, expr);

  /* In all other cases of related types, make a NOP_EXPR.  */
  else if (TYPE_MAIN_VARIANT (type) == TYPE_MAIN_VARIANT (etype)
	   || (code == INTEGER_CST && ecode == INTEGER_CST
	       && (type == TREE_TYPE (etype) || etype == TREE_TYPE (type))))
    return fold (build1 (NOP_EXPR, type, expr));

  switch (code)
    {
    case VOID_TYPE:
      return build1 (CONVERT_EXPR, type, expr);

    case BOOLEAN_TYPE:
      return fold (build1 (NOP_EXPR, type, gnat_truthvalue_conversion (expr)));

    case INTEGER_TYPE:
      if (TYPE_HAS_ACTUAL_BOUNDS_P (type)
	  && (ecode == ARRAY_TYPE || ecode == UNCONSTRAINED_ARRAY_TYPE
	      || (ecode == RECORD_TYPE && TYPE_CONTAINS_TEMPLATE_P (etype))))
	return unchecked_convert (type, expr, false);
      else if (TYPE_BIASED_REPRESENTATION_P (type))
	return fold (build1 (CONVERT_EXPR, type,
			     fold (build2 (MINUS_EXPR, TREE_TYPE (type),
					   convert (TREE_TYPE (type), expr),
					   TYPE_MIN_VALUE (type)))));

      /* ... fall through ... */

    case ENUMERAL_TYPE:
      return fold (convert_to_integer (type, expr));

    case POINTER_TYPE:
    case REFERENCE_TYPE:
      /* If converting between two pointers to records denoting
	 both a template and type, adjust if needed to account
	 for any differing offsets, since one might be negative.  */
      if (TYPE_THIN_POINTER_P (etype) && TYPE_THIN_POINTER_P (type))
	{
	  tree bit_diff
	    = size_diffop (bit_position (TYPE_FIELDS (TREE_TYPE (etype))),
			   bit_position (TYPE_FIELDS (TREE_TYPE (type))));
	  tree byte_diff = size_binop (CEIL_DIV_EXPR, bit_diff,
				       sbitsize_int (BITS_PER_UNIT));

	  expr = build1 (NOP_EXPR, type, expr);
	  TREE_CONSTANT (expr) = TREE_CONSTANT (TREE_OPERAND (expr, 0));
	  if (integer_zerop (byte_diff))
	    return expr;

	  return build_binary_op (PLUS_EXPR, type, expr,
				  fold (convert_to_pointer (type, byte_diff)));
	}

      /* If converting to a thin pointer, handle specially.  */
      if (TYPE_THIN_POINTER_P (type)
	  && TYPE_UNCONSTRAINED_ARRAY (TREE_TYPE (type)))
	return convert_to_thin_pointer (type, expr);

      /* If converting fat pointer to normal pointer, get the pointer to the
	 array and then convert it.  */
      else if (TYPE_FAT_POINTER_P (etype))
	expr = build_component_ref (expr, get_identifier ("P_ARRAY"),
				    NULL_TREE, false);

      return fold (convert_to_pointer (type, expr));

    case REAL_TYPE:
      return fold (convert_to_real (type, expr));

    case RECORD_TYPE:
      if (TYPE_JUSTIFIED_MODULAR_P (type) && !AGGREGATE_TYPE_P (etype))
	return
	  gnat_build_constructor
	    (type, tree_cons (TYPE_FIELDS (type),
			      convert (TREE_TYPE (TYPE_FIELDS (type)), expr),
			      NULL_TREE));

      /* ... fall through ... */

    case ARRAY_TYPE:
      /* In these cases, assume the front-end has validated the conversion.
	 If the conversion is valid, it will be a bit-wise conversion, so
	 it can be viewed as an unchecked conversion.  */
      return unchecked_convert (type, expr, false);

    case UNION_TYPE:
      /* For unchecked unions, just validate that the type is indeed that of
	 a field of the type.  Then make the simple conversion.  */
      if (TYPE_UNCHECKED_UNION_P (type))
	{
	  for (tem = TYPE_FIELDS (type); tem; tem = TREE_CHAIN (tem))
	    {
	      if (TREE_TYPE (tem) == etype)
		return build1 (CONVERT_EXPR, type, expr);
	      else if (TREE_CODE (TREE_TYPE (tem)) == RECORD_TYPE
		       && (TYPE_JUSTIFIED_MODULAR_P (TREE_TYPE (tem))
			   || TYPE_IS_PADDING_P (TREE_TYPE (tem)))
		       && TREE_TYPE (TYPE_FIELDS (TREE_TYPE (tem))) == etype)
		return build1 (CONVERT_EXPR, type,
			       convert (TREE_TYPE (tem), expr));
	    }

	  gcc_unreachable ();
	}
      else
	/* Otherwise, this is a conversion between a tagged type and some
	   subtype, which we have to mark as a UNION_TYPE because of
	   overlapping fields.  */
	return unchecked_convert (type, expr, false);

    case UNCONSTRAINED_ARRAY_TYPE:
      /* If EXPR is a constrained array, take its address, convert it to a
	 fat pointer, and then dereference it.  Likewise if EXPR is a
	 record containing both a template and a constrained array.
	 Note that a record representing a justified modular type
	 always represents a packed constrained array.  */
      if (ecode == ARRAY_TYPE
	  || (ecode == INTEGER_TYPE && TYPE_HAS_ACTUAL_BOUNDS_P (etype))
	  || (ecode == RECORD_TYPE && TYPE_CONTAINS_TEMPLATE_P (etype))
	  || (ecode == RECORD_TYPE && TYPE_JUSTIFIED_MODULAR_P (etype)))
	return
	  build_unary_op
	    (INDIRECT_REF, NULL_TREE,
	     convert_to_fat_pointer (TREE_TYPE (type),
				     build_unary_op (ADDR_EXPR,
						     NULL_TREE, expr)));

      /* Do something very similar for converting one unconstrained
	 array to another.  */
      else if (ecode == UNCONSTRAINED_ARRAY_TYPE)
	return
	  build_unary_op (INDIRECT_REF, NULL_TREE,
			  convert (TREE_TYPE (type),
				   build_unary_op (ADDR_EXPR,
						   NULL_TREE, expr)));
      else
	gcc_unreachable ();

    case COMPLEX_TYPE:
      return fold (convert_to_complex (type, expr));

    default:
      gcc_unreachable ();
    }
}

/* Remove all conversions that are done in EXP.  This includes converting
   from a padded type or to a justified modular type.  If TRUE_ADDRESS
   is true, always return the address of the containing object even if
   the address is not bit-aligned.  */

tree
remove_conversions (tree exp, bool true_address)
{
  switch (TREE_CODE (exp))
    {
    case CONSTRUCTOR:
      if (true_address
	  && TREE_CODE (TREE_TYPE (exp)) == RECORD_TYPE
	  && TYPE_JUSTIFIED_MODULAR_P (TREE_TYPE (exp)))
	return remove_conversions (VEC_index (constructor_elt,
					      CONSTRUCTOR_ELTS (exp), 0)->value,
				   true);
      break;

    case COMPONENT_REF:
      if (TREE_CODE (TREE_TYPE (TREE_OPERAND (exp, 0))) == RECORD_TYPE
	  && TYPE_IS_PADDING_P (TREE_TYPE (TREE_OPERAND (exp, 0))))
	return remove_conversions (TREE_OPERAND (exp, 0), true_address);
      break;

    case VIEW_CONVERT_EXPR:  case NON_LVALUE_EXPR:
    case NOP_EXPR:  case CONVERT_EXPR:
      return remove_conversions (TREE_OPERAND (exp, 0), true_address);

    default:
      break;
    }

  return exp;
}

/* If EXP's type is an UNCONSTRAINED_ARRAY_TYPE, return an expression that
   refers to the underlying array.  If its type has TYPE_CONTAINS_TEMPLATE_P,
   likewise return an expression pointing to the underlying array.  */

tree
maybe_unconstrained_array (tree exp)
{
  enum tree_code code = TREE_CODE (exp);
  tree new;

  switch (TREE_CODE (TREE_TYPE (exp)))
    {
    case UNCONSTRAINED_ARRAY_TYPE:
      if (code == UNCONSTRAINED_ARRAY_REF)
	{
	  new
	    = build_unary_op (INDIRECT_REF, NULL_TREE,
			      build_component_ref (TREE_OPERAND (exp, 0),
						   get_identifier ("P_ARRAY"),
						   NULL_TREE, false));
	  TREE_READONLY (new) = TREE_STATIC (new) = TREE_READONLY (exp);
	  return new;
	}

      else if (code == NULL_EXPR)
	return build1 (NULL_EXPR,
		       TREE_TYPE (TREE_TYPE (TYPE_FIELDS
					     (TREE_TYPE (TREE_TYPE (exp))))),
		       TREE_OPERAND (exp, 0));

    case RECORD_TYPE:
      /* If this is a padded type, convert to the unpadded type and see if
	 it contains a template.  */
      if (TYPE_IS_PADDING_P (TREE_TYPE (exp)))
	{
	  new = convert (TREE_TYPE (TYPE_FIELDS (TREE_TYPE (exp))), exp);
	  if (TREE_CODE (TREE_TYPE (new)) == RECORD_TYPE
	      && TYPE_CONTAINS_TEMPLATE_P (TREE_TYPE (new)))
	    return
	      build_component_ref (new, NULL_TREE,
				   TREE_CHAIN (TYPE_FIELDS (TREE_TYPE (new))),
				   0);
	}
      else if (TYPE_CONTAINS_TEMPLATE_P (TREE_TYPE (exp)))
	return
	  build_component_ref (exp, NULL_TREE,
			       TREE_CHAIN (TYPE_FIELDS (TREE_TYPE (exp))), 0);
      break;

    default:
      break;
    }

  return exp;
}

/* Return an expression that does an unchecked conversion of EXPR to TYPE.
   If NOTRUNC_P is true, truncation operations should be suppressed.  */

tree
unchecked_convert (tree type, tree expr, bool notrunc_p)
{
  tree etype = TREE_TYPE (expr);

  /* If the expression is already the right type, we are done.  */
  if (etype == type)
    return expr;

  /* If both types types are integral just do a normal conversion.
     Likewise for a conversion to an unconstrained array.  */
  if ((((INTEGRAL_TYPE_P (type)
	 && !(TREE_CODE (type) == INTEGER_TYPE
	      && TYPE_VAX_FLOATING_POINT_P (type)))
	|| (POINTER_TYPE_P (type) && ! TYPE_THIN_POINTER_P (type))
	|| (TREE_CODE (type) == RECORD_TYPE
	    && TYPE_JUSTIFIED_MODULAR_P (type)))
       && ((INTEGRAL_TYPE_P (etype)
	    && !(TREE_CODE (etype) == INTEGER_TYPE
		 && TYPE_VAX_FLOATING_POINT_P (etype)))
	   || (POINTER_TYPE_P (etype) && !TYPE_THIN_POINTER_P (etype))
	   || (TREE_CODE (etype) == RECORD_TYPE
	       && TYPE_JUSTIFIED_MODULAR_P (etype))))
      || TREE_CODE (type) == UNCONSTRAINED_ARRAY_TYPE)
    {
      tree rtype = type;

      if (TREE_CODE (etype) == INTEGER_TYPE
	  && TYPE_BIASED_REPRESENTATION_P (etype))
	{
	  tree ntype = copy_type (etype);

	  TYPE_BIASED_REPRESENTATION_P (ntype) = 0;
	  TYPE_MAIN_VARIANT (ntype) = ntype;
	  expr = build1 (NOP_EXPR, ntype, expr);
	}

      if (TREE_CODE (type) == INTEGER_TYPE
	  && TYPE_BIASED_REPRESENTATION_P (type))
	{
	  rtype = copy_type (type);
	  TYPE_BIASED_REPRESENTATION_P (rtype) = 0;
	  TYPE_MAIN_VARIANT (rtype) = rtype;
	}

      expr = convert (rtype, expr);
      if (type != rtype)
	expr = build1 (NOP_EXPR, type, expr);
    }

  /* If we are converting TO an integral type whose precision is not the
     same as its size, first unchecked convert to a record that contains
     an object of the output type.  Then extract the field. */
  else if (INTEGRAL_TYPE_P (type) && TYPE_RM_SIZE (type)
	   && 0 != compare_tree_int (TYPE_RM_SIZE (type),
				     GET_MODE_BITSIZE (TYPE_MODE (type))))
    {
      tree rec_type = make_node (RECORD_TYPE);
      tree field = create_field_decl (get_identifier ("OBJ"), type,
				      rec_type, 1, 0, 0, 0);

      TYPE_FIELDS (rec_type) = field;
      layout_type (rec_type);

      expr = unchecked_convert (rec_type, expr, notrunc_p);
      expr = build_component_ref (expr, NULL_TREE, field, 0);
    }

  /* Similarly for integral input type whose precision is not equal to its
     size.  */
  else if (INTEGRAL_TYPE_P (etype) && TYPE_RM_SIZE (etype)
      && 0 != compare_tree_int (TYPE_RM_SIZE (etype),
				GET_MODE_BITSIZE (TYPE_MODE (etype))))
    {
      tree rec_type = make_node (RECORD_TYPE);
      tree field
	= create_field_decl (get_identifier ("OBJ"), etype, rec_type,
			     1, 0, 0, 0);

      TYPE_FIELDS (rec_type) = field;
      layout_type (rec_type);

      expr = gnat_build_constructor (rec_type, build_tree_list (field, expr));
      expr = unchecked_convert (type, expr, notrunc_p);
    }

  /* We have a special case when we are converting between two
     unconstrained array types.  In that case, take the address,
     convert the fat pointer types, and dereference.  */
  else if (TREE_CODE (etype) == UNCONSTRAINED_ARRAY_TYPE
	   && TREE_CODE (type) == UNCONSTRAINED_ARRAY_TYPE)
    expr = build_unary_op (INDIRECT_REF, NULL_TREE,
			   build1 (VIEW_CONVERT_EXPR, TREE_TYPE (type),
				   build_unary_op (ADDR_EXPR, NULL_TREE,
						   expr)));
  else
    {
      expr = maybe_unconstrained_array (expr);

      /* There's no point in doing two unchecked conversions in a row.  */
      if (TREE_CODE (expr) == VIEW_CONVERT_EXPR)
	expr = TREE_OPERAND (expr, 0);

      etype = TREE_TYPE (expr);
      expr = build1 (VIEW_CONVERT_EXPR, type, expr);
    }

  /* If the result is an integral type whose size is not equal to
     the size of the underlying machine type, sign- or zero-extend
     the result.  We need not do this in the case where the input is
     an integral type of the same precision and signedness or if the output
     is a biased type or if both the input and output are unsigned.  */
  if (!notrunc_p
      && INTEGRAL_TYPE_P (type) && TYPE_RM_SIZE (type)
      && !(TREE_CODE (type) == INTEGER_TYPE
	   && TYPE_BIASED_REPRESENTATION_P (type))
      && 0 != compare_tree_int (TYPE_RM_SIZE (type),
				GET_MODE_BITSIZE (TYPE_MODE (type)))
      && !(INTEGRAL_TYPE_P (etype)
	   && TYPE_UNSIGNED (type) == TYPE_UNSIGNED (etype)
	   && operand_equal_p (TYPE_RM_SIZE (type),
			       (TYPE_RM_SIZE (etype) != 0
				? TYPE_RM_SIZE (etype) : TYPE_SIZE (etype)),
			       0))
      && !(TYPE_UNSIGNED (type) && TYPE_UNSIGNED (etype)))
    {
      tree base_type = gnat_type_for_mode (TYPE_MODE (type),
					   TYPE_UNSIGNED (type));
      tree shift_expr
	= convert (base_type,
		   size_binop (MINUS_EXPR,
			       bitsize_int
			       (GET_MODE_BITSIZE (TYPE_MODE (type))),
			       TYPE_RM_SIZE (type)));
      expr
	= convert (type,
		   build_binary_op (RSHIFT_EXPR, base_type,
				    build_binary_op (LSHIFT_EXPR, base_type,
						     convert (base_type, expr),
						     shift_expr),
				    shift_expr));
    }

  /* An unchecked conversion should never raise Constraint_Error.  The code
     below assumes that GCC's conversion routines overflow the same way that
     the underlying hardware does.  This is probably true.  In the rare case
     when it is false, we can rely on the fact that such conversions are
     erroneous anyway.  */
  if (TREE_CODE (expr) == INTEGER_CST)
    TREE_OVERFLOW (expr) = TREE_CONSTANT_OVERFLOW (expr) = 0;

  /* If the sizes of the types differ and this is an VIEW_CONVERT_EXPR,
     show no longer constant.  */
  if (TREE_CODE (expr) == VIEW_CONVERT_EXPR
      && !operand_equal_p (TYPE_SIZE_UNIT (type), TYPE_SIZE_UNIT (etype),
			   OEP_ONLY_CONST))
    TREE_CONSTANT (expr) = 0;

  return expr;
}

/* Search the chain of currently reachable declarations for a builtin
   FUNCTION_DECL node corresponding to function NAME (an IDENTIFIER_NODE).
   Return the first node found, if any, or NULL_TREE otherwise.  */

tree
builtin_decl_for (tree name __attribute__ ((unused)))
{
  /* ??? not clear yet how to implement this function in tree-ssa, so
     return NULL_TREE for now */
  return NULL_TREE;
}

#include "gt-ada-utils.h"
#include "gtype-ada.h"
