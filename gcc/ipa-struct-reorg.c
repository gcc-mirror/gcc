/* Struct-reorg optimization.
   Copyright (C) 2007, 2008, 2009, 2010 Free Software Foundation, Inc.
   Contributed by Olga Golovanevsky <olga@il.ibm.com>
   (Initial version of this code was developed
   by Caroline Tice and Mostafa Hagog.)

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "ggc.h"
#include "tree.h"
#include "rtl.h"
#include "gimple.h"
#include "tree-inline.h"
#include "tree-flow.h"
#include "tree-flow-inline.h"
#include "langhooks.h"
#include "pointer-set.h"
#include "hashtab.h"
#include "toplev.h"
#include "flags.h"
#include "debug.h"
#include "target.h"
#include "cgraph.h"
#include "diagnostic.h"
#include "timevar.h"
#include "params.h"
#include "fibheap.h"
#include "intl.h"
#include "function.h"
#include "basic-block.h"
#include "tree-iterator.h"
#include "tree-pass.h"
#include "ipa-struct-reorg.h"
#include "opts.h"
#include "ipa-type-escape.h"
#include "tree-dump.h"
#include "gimple.h"

/* This optimization implements structure peeling.

   For example, given a structure type:
   typedef struct
   {
     int a;
     float b;
     int c;
   }str_t;

   it can be peeled into two structure types as follows:

   typedef struct  and  typedef struct
   {                    {
     int a;               float b;
     int c;             } str_t_1;
   }str_t_0;

   or can be fully peeled:

   typedef struct
   {
     int a;
   }str_t_0;

   typedef struct
   {
     float b;
   }str_t_1;

   typedef struct
   {
     int c;
   }str_t_2;

   When structure type is peeled all instances and their accesses
   in the program are updated accordingly. For example, if there is
   array of structures:

   str_t A[N];

   and structure type str_t was peeled into two structures str_t_0
   and str_t_1 as it was shown above, then array A will be replaced
   by two arrays as follows:

   str_t_0 A_0[N];
   str_t_1 A_1[N];

   The field access of field a of element i of array A: A[i].a will be
   replaced by an access to field a of element i of array A_0: A_0[i].a.

   This optimization also supports dynamically allocated arrays.
   If array of structures was allocated by malloc function:

   str_t * p = (str_t *) malloc (sizeof (str_t) * N)

   the allocation site will be replaced to reflect new structure types:

   str_t_0 * p_0 = (str_t_0 *) malloc (sizeof (str_t_0) * N)
   str_t_1 * p_1 = (str_t_1 *) malloc (sizeof (str_t_1) * N)

   The field access through the pointer p[i].a will be changed by p_0[i].a.

   The goal of structure peeling is to improve spatial locality.
   For example, if one of the fields of a structure is accessed frequently
   in the loop:

   for (i = 0; i < N; i++)
   {
     ... = A[i].a;
   }

   the allocation of field a of str_t contiguously in memory will
   increase the chances of fetching the field from cache.

   The analysis part of this optimization is based on the frequency of
   field accesses, which are collected all over the program.
   Then the fields with the frequencies that satisfy the following condition
   get peeled out of the structure:

   freq(f) > C * max_field_freq_in_struct

   where max_field_freq_in_struct is the maximum field frequency
   in the structure. C is a constant defining which portion of
   max_field_freq_in_struct the fields should have in order to be peeled.

   If profiling information is provided, it is used to calculate the
   frequency of field accesses. Otherwise, the structure is fully peeled.

   IPA type-escape analysis is used to determine when it is safe
   to peel a structure.

   The optimization is activated by flag -fipa-struct-reorg.  */

/* New variables created by this optimization.
   When doing struct peeling, each variable of
   the original struct type will be replaced by
   the set of new variables corresponding to
   the new structure types.  */
struct new_var_data {
  /* VAR_DECL for original struct type.  */
  tree orig_var;
  /* Vector of new variables.  */
  VEC(tree, heap) *new_vars;
};

typedef struct new_var_data *new_var;
typedef const struct new_var_data *const_new_var;

/* This structure represents allocation site of the structure.  */
typedef struct alloc_site
{
  gimple stmt;
  d_str str;
} alloc_site_t;

DEF_VEC_O (alloc_site_t);
DEF_VEC_ALLOC_O (alloc_site_t, heap);

/* Allocation sites that belong to the same function.  */
struct func_alloc_sites
{
  tree func;
  /* Vector of allocation sites for function.  */
  VEC (alloc_site_t, heap) *allocs;
};

typedef struct func_alloc_sites *fallocs_t;
typedef const struct func_alloc_sites *const_fallocs_t;

/* All allocation sites in the program.  */
htab_t alloc_sites = NULL;

/* New global variables. Generated once for whole program.  */
htab_t new_global_vars;

/* New local variables. Generated per-function.  */
htab_t new_local_vars;

/* Vector of structures to be transformed.  */
typedef struct data_structure structure;
DEF_VEC_O (structure);
DEF_VEC_ALLOC_O (structure, heap);
VEC (structure, heap) *structures;

/* Forward declarations.  */
static bool is_equal_types (tree, tree);

/* Strip structure TYPE from pointers and arrays.  */

static inline tree
strip_type (tree type)
{
  gcc_assert (TYPE_P (type));

  while (POINTER_TYPE_P (type)
	 || TREE_CODE (type) == ARRAY_TYPE)
    type = TREE_TYPE (type);

  return  type;
}

/* This function returns type of VAR.  */

static inline tree
get_type_of_var (tree var)
{
  if (!var)
    return NULL;

  if (TREE_CODE (var) == PARM_DECL)
      return DECL_ARG_TYPE (var);
  else
    return TREE_TYPE (var);
}

/* Set of actions we do for each newly generated STMT.  */

static inline void
finalize_stmt (gimple stmt)
{
  update_stmt (stmt);
  mark_symbols_for_renaming (stmt);
}

/* This function finalizes STMT and appends it to the list STMTS.  */

static inline void
finalize_stmt_and_append (gimple_seq *stmts, gimple stmt)
{
  gimple_seq_add_stmt (stmts, stmt);
  finalize_stmt (stmt);
}

/* This function returns true if two fields FIELD1 and FIELD2 are 
   semantically equal, and false otherwise.  */

static bool
compare_fields (tree field1, tree field2)
{
  if (DECL_NAME (field1) && DECL_NAME (field2))
    {
      const char *name1 = IDENTIFIER_POINTER (DECL_NAME (field1));
      const char *name2 = IDENTIFIER_POINTER (DECL_NAME (field2));

      gcc_assert (name1 && name2);

      if (strcmp (name1, name2))
	return false;
	
    }
  else if (DECL_NAME (field1) || DECL_NAME (field2))
    return false;

  if (!is_equal_types (TREE_TYPE (field1), TREE_TYPE (field2)))
    return false;

  return true;
}

/* Given structure type SRT_TYPE and field FIELD,
   this function is looking for a field with the same name
   and type as FIELD in STR_TYPE. It returns it if found,
   or NULL_TREE otherwise.  */

static tree
find_field_in_struct_1 (tree str_type, tree field)
{
  tree str_field;

  if (!DECL_NAME (field))
    return NULL;

  for (str_field = TYPE_FIELDS (str_type); str_field;
       str_field = TREE_CHAIN (str_field))
    {

      if (!DECL_NAME (str_field))
	continue;

      if (compare_fields (field, str_field))
	return str_field;
    }

  return NULL_TREE;
}

/* Given a field declaration FIELD_DECL, this function
   returns corresponding field entry in structure STR.  */

static struct field_entry *
find_field_in_struct (d_str str, tree field_decl)
{
  int i;

  tree field = find_field_in_struct_1 (str->decl, field_decl);

  for (i = 0; i < str->num_fields; i++)
    if (str->fields[i].decl == field)
      return &(str->fields[i]);

  return NULL;
}

/* This function checks whether ARG is a result of multiplication
   of some number by STRUCT_SIZE. If yes, the function returns true
   and this number is filled into NUM.  */

static bool
is_result_of_mult (tree arg, tree *num, tree struct_size)
{
  gimple size_def_stmt = SSA_NAME_DEF_STMT (arg);

  /* If the allocation statement was of the form
     D.2229_10 = <alloc_func> (D.2228_9);
     then size_def_stmt can be D.2228_9 = num.3_8 * 8;  */

  if (size_def_stmt && is_gimple_assign (size_def_stmt))
    {
      tree lhs = gimple_assign_lhs (size_def_stmt);

      /* We expect temporary here.  */
      if (!is_gimple_reg (lhs))
	return false;

      if (gimple_assign_rhs_code (size_def_stmt) == MULT_EXPR)
	{
	  tree arg0 = gimple_assign_rhs1 (size_def_stmt);
	  tree arg1 = gimple_assign_rhs2 (size_def_stmt);

	  if (operand_equal_p (arg0, struct_size, OEP_ONLY_CONST))
	    {
	      *num = arg1;
	      return true;
	    }

	  if (operand_equal_p (arg1, struct_size, OEP_ONLY_CONST))
	    {
	      *num = arg0;
	      return true;
	    }
	}
    }

  *num = NULL_TREE;
  return false;
}


/* This function returns true if access ACC corresponds to the pattern
   generated by compiler when an address of element i of an array
   of structures STR_DECL (pointed by p) is calculated (p[i]). If this
   pattern is recognized correctly, this function returns true
   and fills missing fields in ACC. Otherwise it returns false.  */

static bool
decompose_indirect_ref_acc (tree str_decl, struct field_access_site *acc)
{
  tree ref_var;
  tree struct_size, op0, op1;
  tree before_cast;
  enum tree_code rhs_code;

  ref_var = TREE_OPERAND (acc->ref, 0);

  if (TREE_CODE (ref_var) != SSA_NAME)
    return false;

  acc->ref_def_stmt = SSA_NAME_DEF_STMT (ref_var);
  if (!(acc->ref_def_stmt)
      || (gimple_code (acc->ref_def_stmt) != GIMPLE_ASSIGN))
    return false;

  rhs_code = gimple_assign_rhs_code (acc->ref_def_stmt);

  if (rhs_code != PLUS_EXPR
      && rhs_code != MINUS_EXPR
      && rhs_code != POINTER_PLUS_EXPR)
    return false;

  op0 = gimple_assign_rhs1 (acc->ref_def_stmt);
  op1 = gimple_assign_rhs2 (acc->ref_def_stmt);

  if (!is_array_access_through_pointer_and_index (rhs_code, op0, op1,
						 &acc->base, &acc->offset,
						 &acc->cast_stmt))
    return false;

  if (acc->cast_stmt)
    before_cast = SINGLE_SSA_TREE_OPERAND (acc->cast_stmt, SSA_OP_USE);
  else
    before_cast = acc->offset;

  if (!before_cast)
    return false;


  if (SSA_NAME_IS_DEFAULT_DEF (before_cast))
    return false;

  struct_size = TYPE_SIZE_UNIT (str_decl);

  if (!is_result_of_mult (before_cast, &acc->num, struct_size))
    return false;

  return true;
}


/* This function checks whether the access ACC of structure type STR
   is of the form suitable for transformation. If yes, it returns true.
   False otherwise.  */

static bool
decompose_access (tree str_decl, struct field_access_site *acc)
{
  gcc_assert (acc->ref);

  if (TREE_CODE (acc->ref) == INDIRECT_REF)
    return decompose_indirect_ref_acc (str_decl, acc);
  else if (TREE_CODE (acc->ref) == ARRAY_REF)
    return true;
  else if (TREE_CODE (acc->ref) == VAR_DECL)
    return true;

  return false;
}

/* This function creates empty field_access_site node.  */

static inline struct field_access_site *
make_field_acc_node (void)
{
  return XCNEW (struct field_access_site);
}

/* This function returns the structure field access, defined by STMT,
   if it is already in hashtable of function accesses F_ACCS.  */

static struct field_access_site *
is_in_field_accs (gimple stmt, htab_t f_accs)
{
  return (struct field_access_site *)
    htab_find_with_hash (f_accs, stmt, htab_hash_pointer (stmt));
}

/* This function adds an access ACC to the hashtable
   F_ACCS of field accesses.  */

static void
add_field_acc_to_acc_sites (struct field_access_site *acc,
			    htab_t f_accs)
{
  void **slot;

  gcc_assert (!is_in_field_accs (acc->stmt, f_accs));
  slot = htab_find_slot_with_hash (f_accs, acc->stmt,
				   htab_hash_pointer (acc->stmt),
				   INSERT);
  *slot = acc;
}

/* This function adds the VAR to vector of variables of
   an access site defined by statement STMT. If access entry
   with statement STMT does not exist in hashtable of
   accesses ACCS, this function creates it.  */

static void
add_access_to_acc_sites (gimple stmt, tree var, htab_t accs)
{
   struct access_site *acc;

   acc = (struct access_site *)
     htab_find_with_hash (accs,	stmt, htab_hash_pointer (stmt));

   if (!acc)
     {
       void **slot;

       acc = XNEW (struct access_site);
       acc->stmt = stmt;
       if (!is_gimple_debug (stmt))
	 acc->vars = VEC_alloc (tree, heap, 10);
       else
	 acc->vars = NULL;
       slot = htab_find_slot_with_hash (accs, stmt,
					htab_hash_pointer (stmt), INSERT);
       *slot = acc;
     }
   if (!is_gimple_debug (stmt))
     VEC_safe_push (tree, heap, acc->vars, var);
}

/* This function adds NEW_DECL to function
   referenced vars, and marks it for renaming.  */

static void
finalize_var_creation (tree new_decl)
{
  add_referenced_var (new_decl);
  mark_sym_for_renaming (new_decl);
}

/* This function finalizes VAR creation if it is a global VAR_DECL.  */

static void
finalize_global_creation (tree var)
{
  if (TREE_CODE (var) == VAR_DECL
      && is_global_var (var))
    finalize_var_creation (var);
}

/* This function inserts NEW_DECL to varpool.  */

static inline void
insert_global_to_varpool (tree new_decl)
{
  struct varpool_node *new_node;

  new_node = varpool_node (new_decl);
  notice_global_symbol (new_decl);
  varpool_mark_needed_node (new_node);
  varpool_finalize_decl (new_decl);
}

/* This function finalizes the creation of new variables,
   defined by *SLOT->new_vars.  */

static int
finalize_new_vars_creation (void **slot, void *data ATTRIBUTE_UNUSED)
{
  new_var n_var = *(new_var *) slot;
  unsigned i;
  tree var;

  for (i = 0; VEC_iterate (tree, n_var->new_vars, i, var); i++)
    finalize_var_creation (var);
  return 1;
}

/* This function looks for the variable of NEW_TYPE type, stored in VAR.
   It returns it, if found, and NULL_TREE otherwise.  */

static tree
find_var_in_new_vars_vec (new_var var, tree new_type)
{
  tree n_var;
  unsigned i;

  for (i = 0; VEC_iterate (tree, var->new_vars, i, n_var); i++)
    {
      tree type = strip_type(get_type_of_var (n_var));
      gcc_assert (type);

      if (type == new_type)
	return n_var;
    }

  return NULL_TREE;
}

/* This function returns new_var node, the orig_var of which is DECL.
   It looks for new_var's in NEW_VARS_HTAB. If not found,
   the function returns NULL.  */

static new_var
is_in_new_vars_htab (tree decl, htab_t new_vars_htab)
{
  return (new_var) htab_find_with_hash (new_vars_htab, decl,
					DECL_UID (decl));
}

/* Given original variable ORIG_VAR, this function returns
   new variable corresponding to it of NEW_TYPE type. */

static tree
find_new_var_of_type (tree orig_var, tree new_type)
{
  new_var var;
  gcc_assert (orig_var && new_type);

  if (TREE_CODE (orig_var) == SSA_NAME)
    orig_var = SSA_NAME_VAR (orig_var);

  var = is_in_new_vars_htab (orig_var, new_global_vars);
  if (!var)
    var = is_in_new_vars_htab (orig_var, new_local_vars);
  gcc_assert (var);
  return find_var_in_new_vars_vec (var, new_type);
}

/* This function generates stmt:
   res = NUM * sizeof(TYPE) and returns it.
   res is filled into RES.  */

static gimple
gen_size (tree num, tree type, tree *res)
{
  tree struct_size = TYPE_SIZE_UNIT (type);
  HOST_WIDE_INT struct_size_int = TREE_INT_CST_LOW (struct_size);
  gimple new_stmt;

  *res = create_tmp_var (TREE_TYPE (num), NULL);

  if (*res)
    add_referenced_var (*res);

  if (exact_log2 (struct_size_int) == -1)
    {
      tree size = build_int_cst (TREE_TYPE (num), struct_size_int);
      new_stmt = gimple_build_assign (*res, fold_build2 (MULT_EXPR,
							 TREE_TYPE (num),
							 num, size));
    }
  else
    {
      tree C = build_int_cst (TREE_TYPE (num), exact_log2 (struct_size_int));

      new_stmt = gimple_build_assign (*res, fold_build2 (LSHIFT_EXPR,
							 TREE_TYPE (num),
							 num, C));
    }

  finalize_stmt (new_stmt);
  return new_stmt;
}

/* This function generates and returns a statement, that cast variable
   BEFORE_CAST to NEW_TYPE. The cast result variable is stored
   into RES_P. ORIG_CAST_STMT is the original cast statement.  */

static gimple
gen_cast_stmt (tree before_cast, tree new_type, gimple orig_cast_stmt,
	       tree *res_p)
{
  tree lhs, new_lhs;
  gimple new_stmt;

  lhs = gimple_assign_lhs (orig_cast_stmt);
  new_lhs = find_new_var_of_type (lhs, new_type);
  gcc_assert (new_lhs);

  new_stmt = gimple_build_assign_with_ops (NOP_EXPR, new_lhs, before_cast, 0);
  finalize_stmt (new_stmt);
  *res_p = new_lhs;
  return new_stmt;
}

/* This function builds an edge between BB and E->dest and updates
   phi nodes of E->dest. It returns newly created edge.  */

static edge
make_edge_and_fix_phis_of_dest (basic_block bb, edge e)
{
  edge new_e;
  tree arg;
  gimple_stmt_iterator si;

  new_e = make_edge (bb, e->dest, e->flags);

  for (si = gsi_start_phis (new_e->dest); !gsi_end_p (si); gsi_next (&si))
    {
      gimple phi = gsi_stmt (si);
      arg = PHI_ARG_DEF_FROM_EDGE (phi, e);
      add_phi_arg (phi, arg, new_e, gimple_phi_arg_location_from_edge (phi, e));
    }

  return new_e;
}

/* This function inserts NEW_STMT before STMT.  */

static void
insert_before_stmt (gimple stmt, gimple new_stmt)
{
  gimple_stmt_iterator bsi;

  if (!stmt || !new_stmt)
    return;

  bsi = gsi_for_stmt (stmt);
  gsi_insert_before (&bsi, new_stmt, GSI_SAME_STMT);
}

/* Insert NEW_STMTS after STMT.  */

static void
insert_seq_after_stmt (gimple stmt, gimple_seq new_stmts)
{
  gimple_stmt_iterator bsi;

  if (!stmt || !new_stmts)
    return;

  bsi = gsi_for_stmt (stmt);
  gsi_insert_seq_after (&bsi, new_stmts, GSI_SAME_STMT);
}

/* Insert NEW_STMT after STMT.  */

static void
insert_after_stmt (gimple stmt, gimple new_stmt)
{
  gimple_stmt_iterator bsi;

  if (!stmt || !new_stmt)
    return;

  bsi = gsi_for_stmt (stmt);
  gsi_insert_after (&bsi, new_stmt, GSI_SAME_STMT);
}

/* This function returns vector of allocation sites
   that appear in function FN_DECL.  */

static fallocs_t
get_fallocs (tree fn_decl)
{
  return (fallocs_t) htab_find_with_hash (alloc_sites, fn_decl,
					 htab_hash_pointer (fn_decl));
}

/* If ALLOC_STMT is D.2225_7 = <alloc_func> (D.2224_6);
   and it is a part of allocation of a structure,
   then it is usually followed by a cast stmt
   p_8 = (struct str_t *) D.2225_7;
   which is returned by this function.  */

static gimple
get_final_alloc_stmt (gimple alloc_stmt)
{
  gimple final_stmt;
  use_operand_p use_p;
  tree alloc_res;

  if (!alloc_stmt)
    return NULL;

  if (!is_gimple_call (alloc_stmt))
    return NULL;

  alloc_res = gimple_get_lhs (alloc_stmt);

  if (TREE_CODE (alloc_res) != SSA_NAME)
    return NULL;

  if (!single_imm_use (alloc_res, &use_p, &final_stmt))
    return NULL;
  else
    return final_stmt;
}

/* This function returns true if STMT is one of allocation
   sites of function FN_DECL. It returns false otherwise.  */

static bool
is_part_of_malloc (gimple stmt, tree fn_decl)
{
  fallocs_t fallocs = get_fallocs (fn_decl);

  if (fallocs)
    {
      alloc_site_t *call;
      unsigned i;

      for (i = 0; VEC_iterate (alloc_site_t, fallocs->allocs, i, call); i++)
	if (call->stmt == stmt
	    || get_final_alloc_stmt (call->stmt) == stmt)
	  return true;
    }
  return false;
}

/* Auxiliary structure for a lookup over field accesses. */
struct find_stmt_data
{
  bool found;
  gimple stmt;
};

/* This function looks for DATA->stmt among
   the statements involved in the field access,
   defined by SLOT. It stops when it's found. */

static int
find_in_field_accs (void **slot, void *data)
{
  struct field_access_site *f_acc = *(struct field_access_site **) slot;
  gimple stmt = ((struct find_stmt_data *)data)->stmt;

  if (f_acc->stmt == stmt
      || f_acc->ref_def_stmt == stmt
      || f_acc->cast_stmt == stmt)
    {
      ((struct find_stmt_data *)data)->found = true;
      return 0;
    }
  else
    return 1;
}

/* This function checks whether STMT is part of field
   accesses of structure STR. It returns true, if found,
   and false otherwise.  */

static bool
is_part_of_field_access (gimple stmt, d_str str)
{
  int i;

  for (i = 0; i < str->num_fields; i++)
    {
      struct find_stmt_data data;
      data.found = false;
      data.stmt = stmt;

      if (str->fields[i].acc_sites)
	htab_traverse (str->fields[i].acc_sites, find_in_field_accs, &data);

      if (data.found)
	return true;
    }

  return false;
}

/* Auxiliary data for exclude_from_accs function.  */

struct exclude_data
{
  tree fn_decl;
  d_str str;
};

/* This function returns component_ref with the BASE and
   field named FIELD_ID from structure TYPE.  */

static inline tree
build_comp_ref (tree base, tree field_id, tree type)
{
  tree field;
  bool found = false;


  /* Find field of structure type with the same name as field_id. */
  for (field = TYPE_FIELDS (type); field; field = TREE_CHAIN (field))
    {
      if (DECL_NAME (field) == field_id)
	{
	  found = true;
	  break;
	}
    }

  gcc_assert (found);

  return build3 (COMPONENT_REF, TREE_TYPE (field), base, field, NULL_TREE);
}


/* This struct represent data used for walk_tree
   called from function find_pos_in_stmt.
   - ref is a tree to be found,
   - and pos is a pointer that points to ref in stmt.  */
struct ref_pos
{
  tree *pos;
  tree ref;
  tree container;
};


/* This is a callback function for walk_tree, called from
   collect_accesses_in_bb function. DATA is a pointer to ref_pos structure.
   When *TP is equal to DATA->ref, the walk_tree stops,
   and found position, equal to TP, is assigned to DATA->pos.  */

static tree
find_pos_in_stmt_1 (tree *tp, int *walk_subtrees, void * data)
{
  struct walk_stmt_info *wi = (struct walk_stmt_info *) data;
  struct ref_pos *r_pos = (struct ref_pos *) wi->info;
  tree ref = r_pos->ref;
  tree t = *tp;

  if (t == ref || (TREE_CODE (t) == SSA_NAME && SSA_NAME_VAR (t) == ref))
    {
      r_pos->pos = tp;
      return t;
    }

  r_pos->container = t;
  *walk_subtrees = 1;
  return NULL_TREE;
}


/* This function looks for the pointer of REF in STMT,
   It returns it, if found, and NULL otherwise.  */

static tree *
find_pos_in_stmt (gimple stmt, tree ref, struct ref_pos * r_pos)
{
  struct walk_stmt_info wi;

  r_pos->ref = ref;
  r_pos->pos = NULL;
  r_pos->container = NULL_TREE;
  memset (&wi, 0, sizeof (wi));
  wi.info = r_pos;
  walk_gimple_op (stmt, find_pos_in_stmt_1, &wi);

  return r_pos->pos;
}

/* This structure is used to represent array
   or pointer-to wrappers of structure type.
   For example, if type1 is structure type,
   then for type1 ** we generate two type_wrapper
   structures with wrap = 0 each one.
   It's used to unwind the original type up to
   structure type, replace it with the new structure type
   and wrap it back in the opposite order.  */

typedef struct type_wrapper
{
  /* 0 stand for pointer wrapper, and 1 for array wrapper.  */
  bool wrap;

  /* Relevant for arrays as domain or index.  */
  tree domain;
}type_wrapper_t;

DEF_VEC_O (type_wrapper_t);
DEF_VEC_ALLOC_O (type_wrapper_t, heap);

/* This function replace field access ACC by the new
   field access of structure type NEW_TYPE.  */

static void
replace_field_acc (struct field_access_site *acc, tree new_type)
{
  tree ref_var = acc->ref;
  tree new_ref;
  tree lhs, rhs;
  tree *pos;
  tree new_acc;
  tree field_id = DECL_NAME (acc->field_decl);
  VEC (type_wrapper_t, heap) *wrapper = VEC_alloc (type_wrapper_t, heap, 10);
  type_wrapper_t *wr_p = NULL;
  struct ref_pos r_pos;

  while (TREE_CODE (ref_var) == INDIRECT_REF
	 || TREE_CODE (ref_var) == ARRAY_REF)
    {
      type_wrapper_t wr;

      if ( TREE_CODE (ref_var) == INDIRECT_REF)
	{
	  wr.wrap = 0;
	  wr.domain = 0;
	}
      else
	{
	  wr.wrap = 1;
	  wr.domain = TREE_OPERAND (ref_var, 1);
	}

      VEC_safe_push (type_wrapper_t, heap, wrapper, &wr);
      ref_var = TREE_OPERAND (ref_var, 0);
    }

  new_ref = find_new_var_of_type (ref_var, new_type);
  finalize_global_creation (new_ref);

  while (VEC_length (type_wrapper_t, wrapper) != 0)
    {
      tree type = TREE_TYPE (TREE_TYPE (new_ref));

      wr_p = VEC_last (type_wrapper_t, wrapper);
      if (wr_p->wrap) /* Array.  */
	new_ref = build4 (ARRAY_REF, type, new_ref,
			  wr_p->domain, NULL_TREE, NULL_TREE);
      else /* Pointer.  */
	new_ref = build1 (INDIRECT_REF, type, new_ref);
      VEC_pop (type_wrapper_t, wrapper);
    }

  new_acc = build_comp_ref (new_ref, field_id, new_type);
  VEC_free (type_wrapper_t, heap, wrapper);

  if (is_gimple_assign (acc->stmt))
    {
      lhs = gimple_assign_lhs (acc->stmt);
      rhs = gimple_assign_rhs1 (acc->stmt);

      if (lhs == acc->comp_ref)
	gimple_assign_set_lhs (acc->stmt, new_acc);
      else if (rhs == acc->comp_ref)
	gimple_assign_set_rhs1 (acc->stmt, new_acc);
      else
	{
	  pos = find_pos_in_stmt (acc->stmt, acc->comp_ref, &r_pos);
	  gcc_assert (pos);
	  *pos = new_acc;
	}
    }
  else
    {
      pos = find_pos_in_stmt (acc->stmt, acc->comp_ref, &r_pos);
      gcc_assert (pos);
      *pos = new_acc;
    }

  finalize_stmt (acc->stmt);
}

/* This function replace field access ACC by a new field access
   of structure type NEW_TYPE.  */

static void
replace_field_access_stmt (struct field_access_site *acc, tree new_type)
{

  if (TREE_CODE (acc->ref) == INDIRECT_REF
      ||TREE_CODE (acc->ref) == ARRAY_REF
      ||TREE_CODE (acc->ref) == VAR_DECL)
    replace_field_acc (acc, new_type);
  else
    gcc_unreachable ();
}

/* This function looks for d_str, represented by TYPE, in the structures
   vector. If found, it returns an index of found structure. Otherwise
   it returns a length of the structures vector.  */

static unsigned
find_structure (tree type)
{
  d_str str;
  unsigned i;

  type = TYPE_MAIN_VARIANT (type);

  for (i = 0; VEC_iterate (structure, structures, i, str); i++)
    if (is_equal_types (str->decl, type))
      return i;

  return VEC_length (structure, structures);
}

/* In this function we create new statements that have the same
   form as ORIG_STMT, but of type NEW_TYPE. The statements
   treated by this function are simple assignments,
   like assignments:  p.8_7 = p; or statements with rhs of
   tree codes PLUS_EXPR and MINUS_EXPR.  */

static gimple
create_base_plus_offset (gimple orig_stmt, tree new_type, tree offset)
{
  tree lhs;
  tree new_lhs;
  gimple new_stmt;
  tree new_op0 = NULL_TREE, new_op1 = NULL_TREE;

  lhs = gimple_assign_lhs (orig_stmt);

  gcc_assert (TREE_CODE (lhs) == VAR_DECL
	      || TREE_CODE (lhs) == SSA_NAME);

  new_lhs = find_new_var_of_type (lhs, new_type);
  gcc_assert (new_lhs);
  finalize_var_creation (new_lhs);

  switch (gimple_assign_rhs_code (orig_stmt))
    {
    case PLUS_EXPR:
    case MINUS_EXPR:
    case POINTER_PLUS_EXPR:
      {
	tree op0 = gimple_assign_rhs1 (orig_stmt);
	tree op1 = gimple_assign_rhs2 (orig_stmt);
	unsigned str0, str1;
	unsigned length = VEC_length (structure, structures);


	str0 = find_structure (strip_type (get_type_of_var (op0)));
	str1 = find_structure (strip_type (get_type_of_var (op1)));
	gcc_assert ((str0 != length) || (str1 != length));

	if (str0 != length)
	  new_op0 = find_new_var_of_type (op0, new_type);
	if (str1 != length)
	  new_op1 = find_new_var_of_type (op1, new_type);

	if (!new_op0)
	  new_op0 = offset;
	if (!new_op1)
	  new_op1 = offset;
      }
      break;

    default:
      gcc_unreachable();
    }

  new_stmt = gimple_build_assign_with_ops (gimple_assign_rhs_code (orig_stmt),
                                           new_lhs, new_op0, new_op1);
  finalize_stmt (new_stmt);

  return new_stmt;
}

/* Given a field access F_ACC of the FIELD, this function
   replaces it by the new field access.  */

static void
create_new_field_access (struct field_access_site *f_acc,
			 struct field_entry field)
{
  tree new_type = field.field_mapping;
  gimple new_stmt;
  tree size_res;
  gimple mult_stmt;
  gimple cast_stmt;
  tree cast_res = NULL;

  if (f_acc->num)
    {
      mult_stmt = gen_size (f_acc->num, new_type, &size_res);
      insert_before_stmt (f_acc->ref_def_stmt, mult_stmt);
    }

  if (f_acc->cast_stmt)
    {
      cast_stmt = gen_cast_stmt (size_res, new_type,
				 f_acc->cast_stmt, &cast_res);
      insert_after_stmt (f_acc->cast_stmt, cast_stmt);
    }

  if (f_acc->ref_def_stmt)
    {
      tree offset;
      if (cast_res)
	offset = cast_res;
      else
	offset = size_res;

      new_stmt = create_base_plus_offset (f_acc->ref_def_stmt,
					  new_type, offset);
      insert_after_stmt (f_acc->ref_def_stmt, new_stmt);
    }

  /* In stmt D.2163_19 = D.2162_18->b; we replace variable
   D.2162_18 by an appropriate variable of new_type type.  */
  replace_field_access_stmt (f_acc, new_type);
}

/* This function creates a new condition statement
   corresponding to the original COND_STMT, adds new basic block
   and redirects condition edges. NEW_VAR is a new condition
   variable located in the condition statement at the position POS.  */

static void
create_new_stmts_for_cond_expr_1 (tree new_var, gimple cond_stmt, unsigned pos)
{
  gimple new_stmt;
  edge true_e = NULL, false_e = NULL;
  basic_block new_bb;
  gimple_stmt_iterator si;

  extract_true_false_edges_from_block (gimple_bb (cond_stmt),
				       &true_e, &false_e);

  new_stmt = gimple_build_cond (gimple_cond_code (cond_stmt),
			       pos == 0 ? new_var : gimple_cond_lhs (cond_stmt),
			       pos == 1 ? new_var : gimple_cond_rhs (cond_stmt),
			       NULL_TREE,
			       NULL_TREE);

  finalize_stmt (new_stmt);

  /* Create new basic block after bb.  */
  new_bb = create_empty_bb (gimple_bb (cond_stmt));

  /* Add new condition stmt to the new_bb.  */
  si = gsi_start_bb (new_bb);
  gsi_insert_after (&si, new_stmt, GSI_NEW_STMT);

  /* Create false and true edges from new_bb.  */
  make_edge_and_fix_phis_of_dest (new_bb, true_e);
  make_edge_and_fix_phis_of_dest (new_bb, false_e);

  /* Redirect one of original edges to point to new_bb.  */
  if (gimple_cond_code (cond_stmt) == NE_EXPR)
    redirect_edge_succ (true_e, new_bb);
  else
    redirect_edge_succ (false_e, new_bb);
}

/* This function creates new condition statements corresponding
   to original condition STMT, one for each new type, and
   recursively redirect edges to newly generated basic blocks.  */

static void
create_new_stmts_for_cond_expr (gimple stmt)
{
  tree arg0, arg1, arg;
  unsigned str0, str1;
  bool s0, s1;
  d_str str;
  tree type;
  unsigned pos;
  int i;
  unsigned length = VEC_length (structure, structures);

  gcc_assert (gimple_cond_code (stmt) == EQ_EXPR
	      || gimple_cond_code (stmt) == NE_EXPR);

  arg0 = gimple_cond_lhs (stmt);
  arg1 = gimple_cond_rhs (stmt);

  str0 = find_structure (strip_type (get_type_of_var (arg0)));
  str1 = find_structure (strip_type (get_type_of_var (arg1)));

  s0 = (str0 != length) ? true : false;
  s1 = (str1 != length) ? true : false;

  gcc_assert (s0 || s1);
  /* For now we allow only comparison with 0 or NULL.  */
  gcc_assert (integer_zerop (arg0) || integer_zerop (arg1));

  str = integer_zerop (arg0) ?
    VEC_index (structure, structures, str1):
    VEC_index (structure, structures, str0);
  arg = integer_zerop (arg0) ? arg1 : arg0;
  pos = integer_zerop (arg0) ? 1 : 0;

  for (i = 0; VEC_iterate (tree, str->new_types, i, type); i++)
    {
      tree new_arg;

      new_arg = find_new_var_of_type (arg, type);
      create_new_stmts_for_cond_expr_1 (new_arg, stmt, pos);
    }
}

/* This function looks for VAR in STMT, and replace it with NEW_VAR.
   If needed, it wraps NEW_VAR in pointers and indirect references
   before insertion.  */

static void
insert_new_var_in_stmt (gimple stmt, tree var, tree new_var)
{
  struct ref_pos r_pos;
  tree *pos;

  pos = find_pos_in_stmt (stmt, var, &r_pos);
  gcc_assert (pos);

  while (r_pos.container && (TREE_CODE(r_pos.container) == INDIRECT_REF
			     || TREE_CODE(r_pos.container) == ADDR_EXPR))
    {
      tree type = TREE_TYPE (TREE_TYPE (new_var));

      if (TREE_CODE(r_pos.container) == INDIRECT_REF)
	new_var = build1 (INDIRECT_REF, type, new_var);
      else
	new_var = build_fold_addr_expr (new_var);
      pos = find_pos_in_stmt (stmt, r_pos.container, &r_pos);
    }

  *pos = new_var;
}


/* Create a new general access to replace original access ACC
   for structure type NEW_TYPE.  */

static gimple
create_general_new_stmt (struct access_site *acc, tree new_type)
{
  gimple old_stmt = acc->stmt;
  tree var;
  gimple new_stmt = gimple_copy (old_stmt);
  unsigned i;

  /* We are really building a new stmt, clear the virtual operands.  */
  if (gimple_has_mem_ops (new_stmt))
    {
      gimple_set_vuse (new_stmt, NULL_TREE);
      gimple_set_vdef (new_stmt, NULL_TREE);
    }

  for (i = 0; VEC_iterate (tree, acc->vars, i, var); i++)
    {
      tree new_var = find_new_var_of_type (var, new_type);
      tree lhs, rhs = NULL_TREE;

      gcc_assert (new_var);
      finalize_var_creation (new_var);

      if (is_gimple_assign (new_stmt))
	{
	  lhs = gimple_assign_lhs (new_stmt);

	  if (TREE_CODE (lhs) == SSA_NAME)
	    lhs = SSA_NAME_VAR (lhs);
	  if (gimple_assign_rhs_code (new_stmt) == SSA_NAME)
	    rhs = SSA_NAME_VAR (gimple_assign_rhs1 (new_stmt));

	  /* It can happen that rhs is a constructor.
	   Then we have to replace it to be of new_type.  */
	  if (gimple_assign_rhs_code (new_stmt) == CONSTRUCTOR)
	    {
	      /* Dealing only with empty constructors right now.  */
	      gcc_assert (VEC_empty (constructor_elt,
				     CONSTRUCTOR_ELTS (rhs)));
	      rhs = build_constructor (new_type, 0);
	      gimple_assign_set_rhs1 (new_stmt, rhs);
	    }

	  if (lhs == var)
	    gimple_assign_set_lhs (new_stmt, new_var);
	  else if (rhs == var)
	    gimple_assign_set_rhs1 (new_stmt, new_var);
	  else
	    insert_new_var_in_stmt (new_stmt, var, new_var);
	}
      else
	insert_new_var_in_stmt (new_stmt, var, new_var);
    }

  finalize_stmt (new_stmt);
  return new_stmt;
}

/* For each new type in STR this function creates new general accesses
   corresponding to the original access ACC.  */

static void
create_new_stmts_for_general_acc (struct access_site *acc, d_str str)
{
  tree type;
  gimple stmt = acc->stmt;
  unsigned i;

  for (i = 0; VEC_iterate (tree, str->new_types, i, type); i++)
    {
      gimple new_stmt;

      new_stmt = create_general_new_stmt (acc, type);
      insert_after_stmt (stmt, new_stmt);
    }
}

/* This function creates a new general access of structure STR
   to replace the access ACC.  */

static void
create_new_general_access (struct access_site *acc, d_str str)
{
  gimple stmt = acc->stmt;
  switch (gimple_code (stmt))
    {
    case GIMPLE_COND:
      create_new_stmts_for_cond_expr (stmt);
      break;

    case GIMPLE_DEBUG:
      /* It is very hard to maintain usable debug info after struct peeling,
	 for now just reset all debug stmts referencing objects that have
	 been peeled.  */
      gimple_debug_bind_reset_value (stmt);
      update_stmt (stmt);
      break;

    default:
      create_new_stmts_for_general_acc (acc, str);
    }
}

/* Auxiliary data for creation of accesses.  */
struct create_acc_data
{
  basic_block bb;
  d_str str;
  int field_index;
};

/* This function creates a new general access, defined by SLOT.
   DATA is a pointer to create_acc_data structure.  */

static int
create_new_acc (void **slot, void *data)
{
  struct access_site *acc = *(struct access_site **) slot;
  basic_block bb = ((struct create_acc_data *)data)->bb;
  d_str str = ((struct create_acc_data *)data)->str;

  if (gimple_bb (acc->stmt) == bb)
    create_new_general_access (acc, str);
  return 1;
}

/* This function creates a new field access, defined by SLOT.
   DATA is a pointer to create_acc_data structure.  */

static int
create_new_field_acc (void **slot, void *data)
{
  struct field_access_site *f_acc = *(struct field_access_site **) slot;
  basic_block bb = ((struct create_acc_data *)data)->bb;
  d_str str = ((struct create_acc_data *)data)->str;
  int i = ((struct create_acc_data *)data)->field_index;

  if (gimple_bb (f_acc->stmt) == bb)
    create_new_field_access (f_acc, str->fields[i]);
  return 1;
}

/* This function creates new accesses for the structure
   type STR in basic block BB.  */

static void
create_new_accs_for_struct (d_str str, basic_block bb)
{
  int i;
  struct create_acc_data dt;

  dt.str = str;
  dt.bb = bb;
  dt.field_index = -1;

  for (i = 0; i < str->num_fields; i++)
    {
      dt.field_index = i;

      if (str->fields[i].acc_sites)
	htab_traverse (str->fields[i].acc_sites,
		       create_new_field_acc, &dt);
    }
  if (str->accs)
    htab_traverse (str->accs, create_new_acc, &dt);
}

/* This function inserts new variables from new_var,
   defined by SLOT, into varpool.  */

static int
update_varpool_with_new_var (void **slot, void *data ATTRIBUTE_UNUSED)
{
  new_var n_var = *(new_var *) slot;
  tree var;
  unsigned i;

  for (i = 0; VEC_iterate (tree, n_var->new_vars, i, var); i++)
    insert_global_to_varpool (var);
  return 1;
}

/* This function prints a field access site, defined by SLOT.  */

static int
dump_field_acc (void **slot, void *data ATTRIBUTE_UNUSED)
{
  struct field_access_site *f_acc =
    *(struct field_access_site **) slot;

  fprintf(dump_file, "\n");
  if (f_acc->stmt)
    print_gimple_stmt (dump_file, f_acc->stmt, 0, 0);
  if (f_acc->ref_def_stmt)
    print_gimple_stmt (dump_file, f_acc->ref_def_stmt, 0, 0);
  if (f_acc->cast_stmt)
    print_gimple_stmt (dump_file, f_acc->cast_stmt, 0, 0);
  return 1;
}

/* Print field accesses from hashtable F_ACCS.  */

static void
dump_field_acc_sites (htab_t f_accs)
{
  if (!dump_file)
    return;

  if (f_accs)
    htab_traverse (f_accs, dump_field_acc, NULL);
}

/* Hash value for fallocs_t.  */

static hashval_t
malloc_hash (const void *x)
{
  return htab_hash_pointer (((const_fallocs_t)x)->func);
}

/* This function returns nonzero if function of func_alloc_sites' X
   is equal to Y.  */

static int
malloc_eq (const void *x, const void *y)
{
  return ((const_fallocs_t)x)->func == (const_tree)y;
}

/* This function is a callback for traversal over a structure accesses.
   It frees an access represented by SLOT.  */

static int
free_accs (void **slot, void *data ATTRIBUTE_UNUSED)
{
  struct access_site * acc = *(struct access_site **) slot;

  VEC_free (tree, heap, acc->vars);
  free (acc);
  return 1;
}

/* This is a callback function for traversal over field accesses.
   It frees a field access represented by SLOT.  */

static int
free_field_accs (void **slot, void *data ATTRIBUTE_UNUSED)
{
  struct field_access_site *f_acc = *(struct field_access_site **) slot;

  free (f_acc);
  return 1;
}

/* This function inserts TYPE into vector of UNSUITABLE_TYPES,
   if it is not there yet.  */

static void
add_unsuitable_type (VEC (tree, heap) **unsuitable_types, tree type)
{
  unsigned i;
  tree t;

  if (!type)
    return;

  type = TYPE_MAIN_VARIANT (type);

  for (i = 0; VEC_iterate (tree, *unsuitable_types, i, t); i++)
    if (is_equal_types (t, type))
      break;

  if (i == VEC_length (tree, *unsuitable_types))
    VEC_safe_push (tree, heap, *unsuitable_types, type);
}

/* Given a type TYPE, this function returns the name of the type.  */

static const char *
get_type_name (tree type)
{
  if (! TYPE_NAME (type))
    return NULL;

  if (TREE_CODE (TYPE_NAME (type)) == IDENTIFIER_NODE)
    return IDENTIFIER_POINTER (TYPE_NAME (type));
  else if (TREE_CODE (TYPE_NAME (type)) == TYPE_DECL
	   && DECL_NAME (TYPE_NAME (type)))
    return IDENTIFIER_POINTER (DECL_NAME (TYPE_NAME (type)));
  else
    return NULL;
}

/* This function is a temporary hack to overcome the types problem.
   When several compilation units are compiled together
   with -combine, the TYPE_MAIN_VARIANT of the same type
   can appear differently in different compilation units.
   Therefore this function first compares type names.
   If there are no names, structure bodies are recursively
   compared.  */

static bool
is_equal_types (tree type1, tree type2)
{
  const char * name1,* name2;

  if ((!type1 && type2)
      ||(!type2 && type1))
    return false;

  if (!type1 && !type2)
    return true;

  if (TREE_CODE (type1) != TREE_CODE (type2))
    return false;

  if (type1 == type2)
      return true;

  if (TYPE_MAIN_VARIANT (type1) == TYPE_MAIN_VARIANT (type2))
      return true;

  name1 = get_type_name (type1);
  name2 = get_type_name (type2);

  if (name1 && name2)
    return strcmp (name1, name2) == 0;

  switch (TREE_CODE (type1))
    {
    case POINTER_TYPE:
    case REFERENCE_TYPE:
      {
	return is_equal_types (TREE_TYPE (type1), TREE_TYPE (type2));
      }
      break;

    case RECORD_TYPE:
    case UNION_TYPE:
    case QUAL_UNION_TYPE:
    case ENUMERAL_TYPE:
      {
	tree field1, field2;

	/* Compare fields of structure.  */
	for (field1 = TYPE_FIELDS (type1), field2 = TYPE_FIELDS (type2);
	     field1 && field2;
	     field1 = TREE_CHAIN (field1), field2 = TREE_CHAIN (field2))
	  {
	    if (!compare_fields (field1, field2))
	      return false;
	  }
	if (field1 || field2)
	  return false;
	else
	  return true;
      }
      break;

    case INTEGER_TYPE:
      {
	if (TYPE_UNSIGNED (type1) == TYPE_UNSIGNED (type2)
	    && TYPE_PRECISION (type1) == TYPE_PRECISION (type2))
	  return true;
      }
      break;

    case ARRAY_TYPE:
      {
	tree d1, d2;
	tree max1, min1, max2, min2;

	if (!is_equal_types (TREE_TYPE (type1), TREE_TYPE (type2)))
	  return false;

	d1 = TYPE_DOMAIN (type1);
	d2 = TYPE_DOMAIN (type2);

	if (!d1 || !d2)
	  return false;

	max1 = TYPE_MAX_VALUE (d1);
	max2 = TYPE_MAX_VALUE (d2);
	min1 = TYPE_MIN_VALUE (d1);
	min2 = TYPE_MIN_VALUE (d2);

	if (max1 && max2 && min1 && min2
	    && TREE_CODE (max1) == TREE_CODE (max2)
	    && TREE_CODE (max1) == INTEGER_CST
	    && TREE_CODE (min1) == TREE_CODE (min2)
	    && TREE_CODE (min1) == INTEGER_CST
	    && tree_int_cst_equal (max1, max2)
	    && tree_int_cst_equal (min1, min2))
	  return true;
      }
      break;

    default:
	gcc_unreachable();
    }

  return false;
}

/* This function free non-field accesses from hashtable ACCS.  */

static void
free_accesses (htab_t accs)
{
  if (accs)
    htab_traverse (accs, free_accs, NULL);
  htab_delete (accs);
}

/* This function free field accesses hashtable F_ACCS.  */

static void
free_field_accesses (htab_t f_accs)
{
  if (f_accs)
    htab_traverse (f_accs, free_field_accs, NULL);
  htab_delete (f_accs);
}

/* Update call graph with new edge generated by new MALLOC_STMT.
   The edge origin is CONTEXT function.  */

static void
update_cgraph_with_malloc_call (gimple malloc_stmt, tree context)
{
  struct cgraph_node *src, *dest;
  tree malloc_fn_decl;

  if (!malloc_stmt)
    return;

  malloc_fn_decl = gimple_call_fndecl (malloc_stmt);

  src = cgraph_node (context);
  dest = cgraph_node (malloc_fn_decl);
  cgraph_create_edge (src, dest, malloc_stmt,
		      gimple_bb (malloc_stmt)->count,
		      compute_call_stmt_bb_frequency
		        (context, gimple_bb (malloc_stmt)),
		      gimple_bb (malloc_stmt)->loop_depth);
}

/* This function generates set of statements required
   to allocate number NUM of structures of type NEW_TYPE.
   The statements are stored in NEW_STMTS. The statement that contain
   call to malloc is returned. MALLOC_STMT is an original call to malloc.  */

static gimple
create_new_malloc (gimple malloc_stmt, tree new_type, gimple_seq *new_stmts,
		   tree num)
{
  tree new_malloc_size;
  tree malloc_fn_decl;
  gimple new_stmt;
  tree malloc_res;
  gimple call_stmt, final_stmt;
  tree cast_res;

  gcc_assert (num && malloc_stmt && new_type);
  *new_stmts = gimple_seq_alloc ();

  /* Generate argument to malloc as multiplication of num
     and size of new_type.  */
  new_stmt = gen_size (num, new_type, &new_malloc_size);
  gimple_seq_add_stmt (new_stmts, new_stmt);

  /* Generate new call for malloc.  */
  malloc_res = create_tmp_var (ptr_type_node, NULL);
  add_referenced_var (malloc_res);

  malloc_fn_decl = gimple_call_fndecl (malloc_stmt);
  call_stmt = gimple_build_call (malloc_fn_decl, 1, new_malloc_size);
  gimple_call_set_lhs (call_stmt, malloc_res);
  finalize_stmt_and_append (new_stmts, call_stmt);

  /* Create new cast statement. */
  final_stmt = get_final_alloc_stmt (malloc_stmt);
  gcc_assert (final_stmt);
  new_stmt = gen_cast_stmt (malloc_res, new_type, final_stmt, &cast_res);
  gimple_seq_add_stmt (new_stmts, new_stmt);

  return call_stmt;
}

/* This function returns a tree representing
   the number of instances of structure STR_DECL allocated
   by allocation STMT. If new statements are generated,
   they are filled into NEW_STMTS_P.  */

static tree
gen_num_of_structs_in_malloc (gimple stmt, tree str_decl,
			      gimple_seq *new_stmts_p)
{
  tree arg;
  tree struct_size;
  HOST_WIDE_INT struct_size_int;

  if (!stmt)
    return NULL_TREE;

  /* Get malloc argument.  */
  if (!is_gimple_call (stmt))
    return NULL_TREE;

  arg = gimple_call_arg (stmt, 0);

  if (TREE_CODE (arg) != SSA_NAME
      && !TREE_CONSTANT (arg))
    return NULL_TREE;

  struct_size = TYPE_SIZE_UNIT (str_decl);
  struct_size_int = TREE_INT_CST_LOW (struct_size);

  gcc_assert (struct_size);

  if (TREE_CODE (arg) == SSA_NAME)
    {
      tree num;
      gimple div_stmt;

      if (is_result_of_mult (arg, &num, struct_size))
	  return num;

      num = create_tmp_var (integer_type_node, NULL);

      if (num)
	add_referenced_var (num);

      if (exact_log2 (struct_size_int) == -1)
	div_stmt = gimple_build_assign_with_ops (TRUNC_DIV_EXPR, num, arg,
						 struct_size);
      else
	{
	  tree C =  build_int_cst (integer_type_node,
				   exact_log2 (struct_size_int));

	  div_stmt = gimple_build_assign_with_ops (RSHIFT_EXPR, num, arg, C);
	}
      gimple_seq_add_stmt (new_stmts_p, div_stmt);
      finalize_stmt (div_stmt);
      return num;
    }

  if (CONSTANT_CLASS_P (arg)
      && multiple_of_p (TREE_TYPE (struct_size), arg, struct_size))
    return int_const_binop (TRUNC_DIV_EXPR, arg, struct_size, 0);

  return NULL_TREE;
}

/* This function is a callback for traversal on new_var's hashtable.
   SLOT is a pointer to new_var. This function prints to dump_file
   an original variable and all new variables from the new_var
   pointed by *SLOT.  */

static int
dump_new_var (void **slot, void *data ATTRIBUTE_UNUSED)
{
  new_var n_var = *(new_var *) slot;
  tree var_type;
  tree var;
  unsigned i;

  var_type = get_type_of_var (n_var->orig_var);

  fprintf (dump_file, "\nOrig var: ");
  print_generic_expr (dump_file, n_var->orig_var, 0);
  fprintf (dump_file, " of type ");
  print_generic_expr (dump_file, var_type, 0);
  fprintf (dump_file, "\n");

  for (i = 0;
       VEC_iterate (tree, n_var->new_vars, i, var); i++)
    {
      var_type = get_type_of_var (var);

      fprintf (dump_file, "      ");
      print_generic_expr (dump_file, var, 0);
      fprintf (dump_file, " of type ");
      print_generic_expr (dump_file, var_type, 0);
      fprintf (dump_file, "\n");
    }
  return 1;
}

/* This function copies attributes form ORIG_DECL to NEW_DECL.  */

static inline void
copy_decl_attributes (tree new_decl, tree orig_decl)
{

  DECL_ARTIFICIAL (new_decl) = 1;
  DECL_EXTERNAL (new_decl) = DECL_EXTERNAL (orig_decl);
  TREE_STATIC (new_decl) = TREE_STATIC (orig_decl);
  TREE_PUBLIC (new_decl) = TREE_PUBLIC (orig_decl);
  TREE_USED (new_decl) = TREE_USED (orig_decl);
  DECL_CONTEXT (new_decl) = DECL_CONTEXT (orig_decl);
  TREE_THIS_VOLATILE (new_decl) = TREE_THIS_VOLATILE (orig_decl);
  TREE_ADDRESSABLE (new_decl) = TREE_ADDRESSABLE (orig_decl);

  if (TREE_CODE (orig_decl) == VAR_DECL)
    {
      TREE_READONLY (new_decl) = TREE_READONLY (orig_decl);
      DECL_TLS_MODEL (new_decl) = DECL_TLS_MODEL (orig_decl);
    }
}

/* This function wraps NEW_STR_TYPE in pointers or arrays wrapper
   the same way as a structure type is wrapped in DECL.
   It returns the generated type.  */

static inline tree
gen_struct_type (tree decl, tree new_str_type)
{
  tree type_orig = get_type_of_var (decl);
  tree new_type = new_str_type;
  VEC (type_wrapper_t, heap) *wrapper = VEC_alloc (type_wrapper_t, heap, 10);
  type_wrapper_t wr;
  type_wrapper_t *wr_p;

  while (POINTER_TYPE_P (type_orig)
	 || TREE_CODE (type_orig) == ARRAY_TYPE)
    {
      if (POINTER_TYPE_P (type_orig))
	{
	  wr.wrap = 0;
	  wr.domain = NULL_TREE;
	}
      else
	{
	  gcc_assert (TREE_CODE (type_orig) == ARRAY_TYPE);
	  wr.wrap = 1;
	  wr.domain = TYPE_DOMAIN (type_orig);
	}
      VEC_safe_push (type_wrapper_t, heap, wrapper, &wr);
      type_orig = TREE_TYPE (type_orig);
    }

  while (VEC_length (type_wrapper_t, wrapper) != 0)
    {
      wr_p = VEC_last (type_wrapper_t, wrapper);

      if (wr_p->wrap) /* Array.  */
	new_type = build_array_type (new_type, wr_p->domain);
      else /* Pointer.  */
	new_type = build_pointer_type (new_type);

      VEC_pop (type_wrapper_t, wrapper);
    }

  VEC_free (type_wrapper_t, heap, wrapper);
  return new_type;
}

/* This function generates and returns new variable name based on
   ORIG_DECL name, combined with index I.
   The form of the new name is <orig_name>.<I> .  */

static tree
gen_var_name (tree orig_decl, unsigned HOST_WIDE_INT i)
{
  const char *old_name;
  char *prefix;
  char *new_name;

  if (!DECL_NAME (orig_decl)
      || !IDENTIFIER_POINTER (DECL_NAME (orig_decl)))
     return NULL;

  /* If the original variable has a name, create an
     appropriate new name for the new variable.  */

  old_name = IDENTIFIER_POINTER (DECL_NAME (orig_decl));
  prefix = XALLOCAVEC (char, strlen (old_name) + 1);
  strcpy (prefix, old_name);
  ASM_FORMAT_PRIVATE_NAME (new_name, prefix, i);
  return get_identifier (new_name);
}

/* This function adds NEW_NODE to hashtable of new_var's NEW_VARS_HTAB. */

static void
add_to_new_vars_htab (new_var new_node, htab_t new_vars_htab)
{
  void **slot;

  slot = htab_find_slot_with_hash (new_vars_htab, new_node->orig_var,
				   DECL_UID (new_node->orig_var),
				   INSERT);
  *slot = new_node;
}

/* This function creates and returns new_var_data node
   with empty new_vars and orig_var equal to VAR.  */

static new_var
create_new_var_node (tree var, d_str str)
{
  new_var node;

  node = XNEW (struct new_var_data);
  node->orig_var = var;
  node->new_vars = VEC_alloc (tree, heap, VEC_length (tree, str->new_types));
  return node;
}

/* Check whether the type of VAR is potential candidate for peeling.
   Returns true if yes, false otherwise.  If yes, TYPE_P will contain
   candidate type. If VAR is initialized, the type of VAR will be added
   to UNSUITABLE_TYPES.  */

static bool
is_candidate (tree var, tree *type_p, VEC (tree, heap) **unsuitable_types)
{
  tree type;
  bool initialized = false;

  *type_p = NULL;

  if (!var)
    return false;

  /* There is no support of initialized vars.  */
  if (TREE_CODE (var) == VAR_DECL
      && DECL_INITIAL (var) != NULL_TREE)
    initialized = true;

  type = get_type_of_var (var);

  if (type)
    {
      type = TYPE_MAIN_VARIANT (strip_type (type));
      if (TREE_CODE (type) != RECORD_TYPE)
	  return false;
      else
	{
	  if (initialized && unsuitable_types && *unsuitable_types)
	    {
	      if (dump_file)
		{
		  fprintf (dump_file, "The type ");
		  print_generic_expr (dump_file, type, 0);
		  fprintf (dump_file, " is initialized...Excluded.");
		}
	      add_unsuitable_type (unsuitable_types, type);
	    }
	  *type_p = type;
	  return true;
      }
    }
  else
    return false;
}

/* Hash value for field_access_site.  */

static hashval_t
field_acc_hash (const void *x)
{
  return htab_hash_pointer (((const struct field_access_site *)x)->stmt);
}

/* This function returns nonzero if stmt of field_access_site X
   is equal to Y.  */

static int
field_acc_eq (const void *x, const void *y)
{
  return ((const struct field_access_site *)x)->stmt == (const_gimple)y;
}

/* This function prints an access site, defined by SLOT.  */

static int
dump_acc (void **slot, void *data ATTRIBUTE_UNUSED)
{
  struct access_site *acc = *(struct access_site **) slot;
  tree var;
  unsigned i;

  fprintf(dump_file, "\n");
  if (acc->stmt)
    print_gimple_stmt (dump_file, acc->stmt, 0, 0);
  fprintf(dump_file, " : ");

  for (i = 0; VEC_iterate (tree, acc->vars, i, var); i++)
    {
      print_generic_expr (dump_file, var, 0);
      fprintf(dump_file, ", ");
    }
  return 1;
}

/* This function frees memory allocated for structure clusters,
   starting from CLUSTER.  */

static void
free_struct_cluster (struct field_cluster* cluster)
{
  if (cluster)
    {
      if (cluster->fields_in_cluster)
	sbitmap_free (cluster->fields_in_cluster);
      if (cluster->sibling)
	free_struct_cluster (cluster->sibling);
      free (cluster);
    }
}

/* Free all allocated memory under the structure node pointed by D_NODE.  */

static void
free_data_struct (d_str d_node)
{
  int i;

  if (!d_node)
    return;

  if (dump_file)
    {
      fprintf (dump_file, "\nRemoving data structure \"");
      print_generic_expr (dump_file, d_node->decl, 0);
      fprintf (dump_file, "\" from data_struct_list.");
    }

  /* Free all space under d_node.  */
  if (d_node->fields)
    {
      for (i = 0; i < d_node->num_fields; i++)
	free_field_accesses (d_node->fields[i].acc_sites);
      free (d_node->fields);
    }

  if (d_node->accs)
     free_accesses (d_node->accs);

  if (d_node->struct_clustering)
    free_struct_cluster (d_node->struct_clustering);

  if (d_node->new_types)
    VEC_free (tree, heap, d_node->new_types);
}

/* This function creates new general and field accesses in BB.  */

static void
create_new_accesses_in_bb (basic_block bb)
{
  d_str str;
  unsigned i;

  for (i = 0; VEC_iterate (structure, structures, i, str); i++)
    create_new_accs_for_struct (str, bb);
}

/* This function adds allocation sites for peeled structures.
   M_DATA is vector of allocation sites of function CONTEXT.  */

static void
create_new_alloc_sites (fallocs_t m_data, tree context)
{
  alloc_site_t *call;
  unsigned j;

  for (j = 0; VEC_iterate (alloc_site_t, m_data->allocs, j, call); j++)
    {
      gimple stmt = call->stmt;
      d_str str = call->str;
      tree num;
      gimple_seq new_stmts = NULL;
      gimple last_stmt = get_final_alloc_stmt (stmt);
      unsigned i;
      tree type;

      num = gen_num_of_structs_in_malloc (stmt, str->decl, &new_stmts);
      if (new_stmts)
	{
	  gimple last_stmt_tmp = gimple_seq_last_stmt (new_stmts);
	  insert_seq_after_stmt (last_stmt, new_stmts);
	  last_stmt = last_stmt_tmp;
	}

      /* Generate an allocation sites for each new structure type.  */
      for (i = 0; VEC_iterate (tree, str->new_types, i, type); i++)
	{
	  gimple new_malloc_stmt = NULL;
	  gimple last_stmt_tmp = NULL;

	  new_stmts = NULL;
	  new_malloc_stmt = create_new_malloc (stmt, type, &new_stmts, num);
	  last_stmt_tmp = gimple_seq_last_stmt (new_stmts);
	  insert_seq_after_stmt (last_stmt, new_stmts);
	  update_cgraph_with_malloc_call (new_malloc_stmt, context);
	  last_stmt = last_stmt_tmp;
	}
    }
}

/* This function prints new variables from hashtable
   NEW_VARS_HTAB to dump_file.  */

static void
dump_new_vars (htab_t new_vars_htab)
{
  if (!dump_file)
    return;

  if (new_vars_htab)
    htab_traverse (new_vars_htab, dump_new_var, NULL);
}

/* Given an original variable ORIG_DECL of structure type STR,
   this function generates new variables of the types defined
   by STR->new_type. Generated types are saved in new_var node NODE.
   ORIG_DECL should has VAR_DECL tree_code.  */

static void
create_new_var_1 (tree orig_decl, d_str str, new_var node)
{
  unsigned i;
  tree type;

  for (i = 0;
       VEC_iterate (tree, str->new_types, i, type); i++)
    {
      tree new_decl = NULL;
      tree new_name;

      new_name = gen_var_name (orig_decl, i);
      type = gen_struct_type (orig_decl, type);

      if (is_global_var (orig_decl))
	new_decl = build_decl (DECL_SOURCE_LOCATION (orig_decl),
			       VAR_DECL, new_name, type);
      else
	{
	  const char *name = new_name ? IDENTIFIER_POINTER (new_name) : NULL;
	  new_decl = create_tmp_var (type, name);
	}

      copy_decl_attributes (new_decl, orig_decl);
      VEC_safe_push (tree, heap, node->new_vars, new_decl);
    }
}

/* This function creates new variables to
   substitute the original variable VAR_DECL and adds
   them to the new_var's hashtable NEW_VARS_HTAB.  */

static void
create_new_var (tree var_decl, htab_t new_vars_htab)
{
  new_var node;
  d_str str;
  tree type;
  unsigned i;

  if (!var_decl || is_in_new_vars_htab (var_decl, new_vars_htab))
    return;

  if (!is_candidate (var_decl, &type, NULL))
    return;

  i = find_structure (type);
  if (i == VEC_length (structure, structures))
    return;

  str = VEC_index (structure, structures, i);
  node = create_new_var_node (var_decl, str);
  create_new_var_1 (var_decl, str, node);
  add_to_new_vars_htab (node, new_vars_htab);
}

/* Hash value for new_var.  */

static hashval_t
new_var_hash (const void *x)
{
  return DECL_UID (((const_new_var)x)->orig_var);
}

/* This function returns nonzero if orig_var of new_var X 
   and tree Y have equal UIDs.  */

static int
new_var_eq (const void *x, const void *y)
{
  if (DECL_P ((const_tree)y))
    return DECL_UID (((const_new_var)x)->orig_var) == DECL_UID ((const_tree)y);
  else
    return 0;
}

/* This function check whether a structure type represented by STR
   escapes due to ipa-type-escape analysis. If yes, this type is added
   to UNSUITABLE_TYPES vector.  */

static void
check_type_escape (d_str str, VEC (tree, heap) **unsuitable_types)
{
  tree type = str->decl;

  if (!ipa_type_escape_type_contained_p (type))
    {
      if (dump_file)
	{
	  fprintf (dump_file, "\nEscaping type is ");
	  print_generic_expr (dump_file, type, 0);
	}
      add_unsuitable_type (unsuitable_types, type);
    }
}

/* Hash value for access_site.  */

static hashval_t
acc_hash (const void *x)
{
  return htab_hash_pointer (((const struct access_site *)x)->stmt);
}

/* Return nonzero if stmt of access_site X is equal to Y.  */

static int
acc_eq (const void *x, const void *y)
{
  return ((const struct access_site *)x)->stmt == (const_gimple)y;
}

/* Given a structure declaration STRUCT_DECL, and number of fields
   in the structure NUM_FIELDS, this function creates and returns
   corresponding field_entry's.  */

static struct field_entry *
get_fields (tree struct_decl, int num_fields)
{
  struct field_entry *list;
  tree t = TYPE_FIELDS (struct_decl);
  int idx = 0;

  list = XNEWVEC (struct field_entry, num_fields);

  for (idx = 0 ; t; t = TREE_CHAIN (t), idx++)
    if (TREE_CODE (t) == FIELD_DECL)
      {
	list[idx].index = idx;
	list[idx].decl = t;
	list[idx].acc_sites =
	  htab_create (32, field_acc_hash, field_acc_eq, NULL);
	list[idx].count = 0;
	list[idx].field_mapping = NULL_TREE;
      }

  return list;
}

/* Print non-field accesses from hashtable ACCS of structure.  */

static void
dump_access_sites (htab_t accs)
{
  if (!dump_file)
    return;

  if (accs)
    htab_traverse (accs, dump_acc, NULL);
}

/* This function is a callback for alloc_sites hashtable
   traversal. SLOT is a pointer to fallocs_t. This function
   removes all allocations of the structure defined by DATA.  */

static int
remove_str_allocs_in_func (void **slot, void *data)
{
  fallocs_t fallocs = *(fallocs_t *) slot;
  unsigned i = 0;
  alloc_site_t *call;

  while (VEC_iterate (alloc_site_t, fallocs->allocs, i, call))
    {
      if (call->str == (d_str) data)
	VEC_ordered_remove (alloc_site_t, fallocs->allocs, i);
      else
	i++;
    }

  return 1;
}

/* This function remove all entries corresponding to the STR structure
   from alloc_sites hashtable.   */

static void
remove_str_allocs (d_str str)
{
  if (!str)
    return;

  if (alloc_sites)
    htab_traverse (alloc_sites, remove_str_allocs_in_func, str);
}

/* This function removes the structure with index I from structures vector.  */

static void
remove_structure (unsigned i)
{
  d_str str;

  if (i >= VEC_length (structure, structures))
    return;

  str = VEC_index (structure, structures, i);

  /* Before removing the structure str, we have to remove its
     allocations from alloc_sites hashtable.  */
  remove_str_allocs (str);
  free_data_struct (str);
  VEC_ordered_remove (structure, structures, i);
}

/* Currently we support only EQ_EXPR or NE_EXPR conditions.
   COND_STMT is a condition statement to check.  */

static bool
is_safe_cond_expr (gimple cond_stmt)
{
  tree arg0, arg1;
  unsigned str0, str1;
  bool s0, s1;
  unsigned length = VEC_length (structure, structures);

  if (gimple_cond_code (cond_stmt) != EQ_EXPR
      && gimple_cond_code (cond_stmt) != NE_EXPR)
    return false;

  arg0 = gimple_cond_lhs (cond_stmt);
  arg1 = gimple_cond_rhs (cond_stmt);

  str0 = find_structure (strip_type (get_type_of_var (arg0)));
  str1 = find_structure (strip_type (get_type_of_var (arg1)));

  s0 = (str0 != length) ? true : false;
  s1 = (str1 != length) ? true : false;

  if (!s0 && !s1)
    return false;

  /* For now we allow only comparison with 0 or NULL.  */
  if (!integer_zerop (arg0) && !integer_zerop (arg1))
    return false;

  return true;
}

/* This function excludes statements, that are
   part of allocation sites or field accesses, from the
   hashtable of general accesses. SLOT represents general
   access that will be checked. DATA is a pointer to
   exclude_data structure.  */

static int
exclude_from_accs (void **slot, void *data)
{
  struct access_site *acc = *(struct access_site **) slot;
  tree fn_decl = ((struct exclude_data *)data)->fn_decl;
  d_str str = ((struct exclude_data *)data)->str;

  if (is_part_of_malloc (acc->stmt, fn_decl)
      || is_part_of_field_access (acc->stmt, str))
    {
      VEC_free (tree, heap, acc->vars);
      free (acc);
      htab_clear_slot (str->accs, slot);
    }
  return 1;
}

/* Callback function for walk_tree called from collect_accesses_in_bb
   function. DATA is the statement which is analyzed.  */

static tree
get_stmt_accesses (tree *tp, int *walk_subtrees, void *data)
{
  struct walk_stmt_info *wi = (struct walk_stmt_info *) data;
  gimple stmt = (gimple) wi->info;
  tree t = *tp;

  if (!t)
    return NULL;

  switch (TREE_CODE (t))
    {
    case BIT_FIELD_REF:
      {
	tree var = TREE_OPERAND(t, 0);
	tree type = TYPE_MAIN_VARIANT (strip_type (get_type_of_var (var)));
	unsigned i = find_structure (type);

	if (i != VEC_length (structure, structures))
	  {
	    if (is_gimple_debug (stmt))
	      {
		d_str str;

		str = VEC_index (structure, structures, i);
		add_access_to_acc_sites (stmt, NULL, str->accs);
		*walk_subtrees = 0;
		break;
	      }
	    if (dump_file)
	      {
		fprintf (dump_file, "\nThe type ");
		print_generic_expr (dump_file, type, 0);
		fprintf (dump_file, " has bitfield.");
	      }
	    remove_structure (i);
	  }
      }
      break;

    case COMPONENT_REF:
      {
	tree ref = TREE_OPERAND (t, 0);
	tree field_decl = TREE_OPERAND (t, 1);


	if ((TREE_CODE (ref) == INDIRECT_REF
	     || TREE_CODE (ref) == ARRAY_REF
	     || TREE_CODE (ref) == VAR_DECL)
	    && TREE_CODE (field_decl) == FIELD_DECL)
	  {
	    tree type = TYPE_MAIN_VARIANT (TREE_TYPE (ref));
	    unsigned i = find_structure (type);

	    if (i != VEC_length (structure, structures))
	      {
		d_str str = VEC_index (structure, structures, i);
		struct field_entry * field =
		  find_field_in_struct (str, field_decl);

		if (is_gimple_debug (stmt))
		  {
		    add_access_to_acc_sites (stmt, NULL, str->accs);
		    *walk_subtrees = 0;
		    break;
		  }

		if (field)
		  {
		    struct field_access_site *acc = make_field_acc_node ();

		    gcc_assert (acc);

		    acc->stmt = stmt;
		    acc->comp_ref = t;
		    acc->ref = ref;
		    acc->field_decl = field_decl;

		    /* Check whether the access is of the form
		       we can deal with.  */
		    if (!decompose_access (str->decl, acc))
		      {
			if (dump_file)
			  {
			    fprintf (dump_file, "\nThe type ");
			    print_generic_expr (dump_file, type, 0);
			    fprintf (dump_file,
				     " has complicate access in statement ");
			    print_gimple_stmt (dump_file, stmt, 0, 0);
			  }

			remove_structure (i);
			free (acc);
		      }
		    else
		      {
			/* Increase count of field.  */
			basic_block bb = gimple_bb (stmt);
			field->count += bb->count;

			/* Add stmt to the acc_sites of field.  */
			add_field_acc_to_acc_sites (acc, field->acc_sites);
		      }
		    *walk_subtrees = 0;
		  }
	      }
	  }
      }
      break;

    case COND_EXPR:
      {
	tree cond = COND_EXPR_COND (t);
	int i;
	for (i = 0; i < TREE_CODE_LENGTH (TREE_CODE (cond)); i++)
	  {
	    tree t = TREE_OPERAND (cond, i);

	    *walk_subtrees = 1;
	    walk_tree (&t, get_stmt_accesses, data, NULL);
	  }
	*walk_subtrees = 0;
      }
      break;

    case VAR_DECL:
    case SSA_NAME:
      {
	unsigned i;

	if (TREE_CODE (t) == SSA_NAME)
	  t = SSA_NAME_VAR (t);

	i = find_structure (strip_type (get_type_of_var (t)));
	if (i != VEC_length (structure, structures))
	  {
	    d_str str;

	    str = VEC_index (structure, structures, i);
	    add_access_to_acc_sites (stmt, t, str->accs);
	  }
	*walk_subtrees = 0;
      }
      break;

    default:
      return NULL;
    }

  return NULL;
}

/* Free structures hashtable.  */

static void
free_structures (void)
{
  d_str str;
  unsigned i;

  for (i = 0; VEC_iterate (structure, structures, i, str); i++)
    free_data_struct (str);

  VEC_free (structure, heap, structures);
  structures = NULL;
}

/* This function is a callback for traversal over new_var's hashtable.
   SLOT is a pointer to new_var. This function frees memory allocated
   for new_var and pointed by *SLOT.  */

static int
free_new_var (void **slot, void *data ATTRIBUTE_UNUSED)
{
  new_var n_var = *(new_var *) slot;

  /* Free vector of new_vars.  */
  VEC_free (tree, heap, n_var->new_vars);
  free (n_var);
  return 1;
}

/* Free new_vars hashtable NEW_VARS_HTAB.  */

static void
free_new_vars_htab (htab_t new_vars_htab)
{
  if (new_vars_htab)
    htab_traverse (new_vars_htab, free_new_var, NULL);
  htab_delete (new_vars_htab);
  new_vars_htab = NULL;
}

/* This function creates new general and field accesses that appear in cfun.  */

static void
create_new_accesses_for_func (void)
{
  basic_block bb;

  FOR_EACH_BB_FN (bb, cfun)
    create_new_accesses_in_bb (bb);
}

/* Create new allocation sites for the function represented by NODE.  */

static void
create_new_alloc_sites_for_func (struct cgraph_node *node)
{
  fallocs_t fallocs = get_fallocs (node->decl);

  if (fallocs)
    create_new_alloc_sites (fallocs, node->decl);
}

/* For each local variable of structure type from the vector of structures
   this function generates new variable(s) to replace it.  */

static void
create_new_local_vars (void)
{
  tree var;
  referenced_var_iterator rvi;

  new_local_vars = htab_create (num_referenced_vars,
				new_var_hash, new_var_eq, NULL);

  FOR_EACH_REFERENCED_VAR (var, rvi)
    {
      if (!is_global_var (var))
	create_new_var (var, new_local_vars);
    }

  if (new_local_vars)
    htab_traverse (new_local_vars, finalize_new_vars_creation, NULL);
  dump_new_vars (new_local_vars);
}

/* This function prints the SHIFT number of spaces to the DUMP_FILE.  */

static inline void
print_shift (unsigned HOST_WIDE_INT shift)
{
  unsigned HOST_WIDE_INT sh = shift;

  while (sh--)
    fprintf (dump_file, " ");
}

/* This function updates field_mapping of FIELDS in CLUSTER with NEW_TYPE.  */

static inline void
update_fields_mapping (struct field_cluster *cluster, tree new_type,
		       struct field_entry * fields, int num_fields)
{
  int i;

  for (i = 0; i < num_fields; i++)
    if (TEST_BIT (cluster->fields_in_cluster, i))
	fields[i].field_mapping = new_type;
}

/* This functions builds structure with FIELDS,
   NAME and attributes similar to ORIG_STRUCT.
   It returns the newly created structure.  */

static tree
build_basic_struct (tree fields, tree name, tree orig_struct)
{
  tree attributes = NULL_TREE;
  tree ref = 0;
  tree x;

  if (TYPE_ATTRIBUTES (orig_struct))
    attributes = unshare_expr (TYPE_ATTRIBUTES (orig_struct));
  ref = make_node (RECORD_TYPE);
  TYPE_SIZE (ref) = 0;
  decl_attributes (&ref, attributes, (int) ATTR_FLAG_TYPE_IN_PLACE);
  TYPE_PACKED (ref) = TYPE_PACKED (orig_struct);
  for (x = fields; x; x = TREE_CHAIN (x))
    {
      DECL_CONTEXT (x) = ref;
      DECL_PACKED (x) |= TYPE_PACKED (ref);
    }
  TYPE_FIELDS (ref) = fields;
  layout_type (ref);
  TYPE_NAME (ref) = name;

  return ref;
}

/* This function copies FIELDS from CLUSTER into TREE_CHAIN as part
   of preparation for new structure building. NUM_FIELDS is a total
   number of fields in the structure. The function returns newly
   generated fields.  */

static tree
create_fields (struct field_cluster * cluster,
	       struct field_entry * fields, int num_fields)
{
  int i;
  tree new_types = NULL_TREE;
  tree last = NULL_TREE;

  for (i = 0; i < num_fields; i++)
    if (TEST_BIT (cluster->fields_in_cluster, i))
      {
	tree new_decl = unshare_expr (fields[i].decl);

	if (!new_types)
	  new_types = new_decl;
	else
	  TREE_CHAIN (last) = new_decl;
	last = new_decl;
      }

  TREE_CHAIN (last) = NULL_TREE;
  return new_types;

}

/* This function creates a cluster name. The name is based on
   the original structure name, if it is present. It has a form:

   <original_struct_name>_sub.<CLUST_NUM>

   The original structure name is taken from the type of DECL.
   If an original structure name is not present, it's generated to be:

   struct.<STR_NUM>

   The function returns identifier of the new cluster name.  */

static inline tree
gen_cluster_name (tree decl, int clust_num, int str_num)
{
  const char * orig_name = get_type_name (decl);
  char * tmp_name = NULL;
  char * prefix;
  char * new_name;
  size_t len;

  if (!orig_name)
    ASM_FORMAT_PRIVATE_NAME(tmp_name, "struct", str_num);

  len = strlen (tmp_name ? tmp_name : orig_name) + strlen ("_sub");
  prefix = XALLOCAVEC (char, len + 1);
  memcpy (prefix, tmp_name ? tmp_name : orig_name,
	  strlen (tmp_name ? tmp_name : orig_name));
  strcpy (prefix + strlen (tmp_name ? tmp_name : orig_name), "_sub");

  ASM_FORMAT_PRIVATE_NAME (new_name, prefix, clust_num);
  return get_identifier (new_name);
}

/* This function checks whether the structure STR has bitfields.
   If yes, this structure type is added to UNSUITABLE_TYPES vector.  */

static void
check_bitfields (d_str str, VEC (tree, heap) **unsuitable_types)
{
  tree type = str->decl;
  int i;

  for (i = 0; i < str->num_fields; i++)
    if (DECL_BIT_FIELD (str->fields[i].decl))
      {
	add_unsuitable_type (unsuitable_types, type);
	if (dump_file)
	{
	  fprintf (dump_file, "\nType ");
	  print_generic_expr (dump_file, type, 0);
	  fprintf (dump_file, "\nescapes due to bitfield ");
	  print_generic_expr (dump_file, str->fields[i].decl, 0);
	}
	break;
      }
}

/* This function adds to UNSUITABLE_TYPES those types that escape
   due to results of ipa-type-escape analysis. See ipa-type-escape.[c,h].  */

static void
exclude_escaping_types_1 (VEC (tree, heap) **unsuitable_types)
{
  d_str str;
  unsigned i;

  for (i = 0; VEC_iterate (structure, structures, i, str); i++)
    check_type_escape (str, unsuitable_types);
}

/* If a structure type is a return type of any function,
   we cannot transform it. Such type is added to UNSUITABLE_TYPES vector.  */

static void
exclude_returned_types (VEC (tree, heap) **unsuitable_types)
{
  struct cgraph_node *c_node;

  for (c_node = cgraph_nodes; c_node; c_node = c_node->next)
    {
      tree ret_t = TREE_TYPE (TREE_TYPE (c_node->decl));

      if (ret_t)
	{
	  ret_t = strip_type (ret_t);
	  if (TREE_CODE (ret_t) == RECORD_TYPE)
	    {
	      add_unsuitable_type (unsuitable_types, TYPE_MAIN_VARIANT (ret_t));
	      if (dump_file)
		{
		  fprintf (dump_file, "\nThe type \"");
		  print_generic_expr (dump_file, ret_t, 0);
		  fprintf (dump_file,
			   "\" is return type of function...Excluded.");
		}
	    }
	}
    }
}

/* This function looks for parameters of local functions
   which are of structure types, or derived from them (arrays
   of structures, pointers to structures, or their combinations).
   We are not handling peeling of such structures right now.
   The found structures types are added to UNSUITABLE_TYPES vector.  */

static void
exclude_types_passed_to_local_func (VEC (tree, heap) **unsuitable_types)
{
  struct cgraph_node *c_node;

  for (c_node = cgraph_nodes; c_node; c_node = c_node->next)
    if (cgraph_function_body_availability (c_node) == AVAIL_LOCAL)
      {
	tree fn = c_node->decl;
	tree arg;

	for (arg = DECL_ARGUMENTS (fn); arg; arg = TREE_CHAIN (arg))
	  {
	    tree type = TREE_TYPE (arg);

	    type = strip_type (type);
	    if (TREE_CODE (type) == RECORD_TYPE)
	      {
		add_unsuitable_type (unsuitable_types,
				     TYPE_MAIN_VARIANT (type));
		if (dump_file)
		  {
		    fprintf (dump_file, "\nPointer to type \"");
		    print_generic_expr (dump_file, type, 0);
		    fprintf (dump_file,
			     "\" is passed to local function...Excluded.");
		  }
	      }
	  }
      }
}

/* This function analyzes structure form of structures
   potential for transformation. If we are not capable to transform
   structure of some form, we remove it from the structures hashtable.
   Right now we cannot handle nested structs, when nesting is
   through any level of pointers or arrays.

   TBD: release these constrains in future.

   Note, that in this function we suppose that all structures
   in the program are members of the structures hashtable right now,
   without excluding escaping types.  */

static void
check_struct_form (d_str str, VEC (tree, heap) **unsuitable_types)
{
  int i;

  for (i = 0; i < str->num_fields; i++)
    {
      tree f_type = strip_type(TREE_TYPE (str->fields[i].decl));

      if (TREE_CODE (f_type) == RECORD_TYPE)
	{
	  add_unsuitable_type (unsuitable_types, TYPE_MAIN_VARIANT (f_type));
	  add_unsuitable_type (unsuitable_types, str->decl);
	  if (dump_file)
	    {
	      fprintf (dump_file, "\nType ");
	      print_generic_expr (dump_file, f_type, 0);
	      fprintf (dump_file, " is a field in the structure ");
	      print_generic_expr (dump_file, str->decl, 0);
	      fprintf (dump_file, ". Escaping...");
	    }
	}
    }
}

/* This function adds a structure TYPE to the vector of structures,
   if it's not already there.  */

static void
add_structure (tree type)
{
  struct data_structure node;
  unsigned i;
  int num_fields;

  type = TYPE_MAIN_VARIANT (type);

  i = find_structure (type);

  if (i != VEC_length (structure, structures))
    return;

  num_fields = fields_length (type);
  node.decl = type;
  node.num_fields = num_fields;
  node.fields = get_fields (type, num_fields);
  node.struct_clustering = NULL;
  node.accs = htab_create (32, acc_hash, acc_eq, NULL);
  node.new_types = VEC_alloc (tree, heap, num_fields);
  node.count = 0;

  VEC_safe_push (structure, heap, structures, &node);

  if (dump_file)
    {
      fprintf (dump_file, "\nAdding data structure \"");
      print_generic_expr (dump_file, type, 0);
      fprintf (dump_file, "\" to data_struct_list.");
    }
}

/* This function adds an allocation site to alloc_sites hashtable.
   The allocation site appears in STMT of function FN_DECL and
   allocates the structure represented by STR.  */

static void
add_alloc_site (tree fn_decl, gimple stmt, d_str str)
{
  fallocs_t fallocs = NULL;
  alloc_site_t m_call;

  m_call.stmt = stmt;
  m_call.str = str;

  fallocs =
    (fallocs_t) htab_find_with_hash (alloc_sites,
				     fn_decl, htab_hash_pointer (fn_decl));

  if (!fallocs)
    {
      void **slot;

      fallocs = XNEW (struct func_alloc_sites);
      fallocs->func = fn_decl;
      fallocs->allocs = VEC_alloc (alloc_site_t, heap, 1);
      slot = htab_find_slot_with_hash (alloc_sites, fn_decl,
				      htab_hash_pointer (fn_decl), INSERT);
      *slot = fallocs;
    }
  VEC_safe_push (alloc_site_t, heap,
		 fallocs->allocs, &m_call);

  if (dump_file)
    {
      fprintf (dump_file, "\nAdding stmt ");
      print_gimple_stmt (dump_file, stmt, 0, 0);
      fprintf (dump_file, " to list of mallocs.");
    }
}

/* This function returns true if the result of STMT, that contains a call
   to an allocation function, is cast to one of the structure types.
   STMT should be of the form:    T.2 = <alloc_func> (T.1);
   If true, I_P contains an index of an allocated structure.
   Otherwise I_P contains the length of the vector of structures.  */

static bool
is_alloc_of_struct (gimple stmt, unsigned *i_p)
{
  tree lhs;
  tree type;
  gimple final_stmt;

  final_stmt = get_final_alloc_stmt (stmt);

  if (!final_stmt)
    return false;

  /* final_stmt should be of the form:
     T.3 = (struct_type *) T.2; */

  if (gimple_code (final_stmt) != GIMPLE_ASSIGN)
    return false;

  lhs = gimple_assign_lhs (final_stmt);

  type = get_type_of_var (lhs);

  if (!type)
    return false;

  if (!POINTER_TYPE_P (type)
      || TREE_CODE (strip_type (type)) != RECORD_TYPE)
    return false;

  *i_p = find_structure (strip_type (type));

  if (*i_p == VEC_length (structure, structures))
    return false;

  return true;
}

/* This function prints non-field and field accesses
   of the structure STR.  */

static void
dump_accs (d_str str)
{
  int i;

  fprintf (dump_file, "\nAccess sites of struct ");
  print_generic_expr (dump_file, str->decl, 0);

  for (i = 0; i < str->num_fields; i++)
    {
      fprintf (dump_file, "\nAccess site of field ");
      print_generic_expr (dump_file, str->fields[i].decl, 0);
      dump_field_acc_sites (str->fields[i].acc_sites);
      fprintf (dump_file, ":\n");
    }
  fprintf (dump_file, "\nGeneral access sites\n");
  dump_access_sites (str->accs);
}

/* This function checks whether an access statement, pointed by SLOT,
   is a condition we are capable to transform.  It returns false if not,
   setting bool *DATA to false.  */

static int
safe_cond_expr_check (void **slot, void *data)
{
  struct access_site *acc = *(struct access_site **) slot;

  if (gimple_code (acc->stmt) == GIMPLE_COND
      && !is_safe_cond_expr (acc->stmt))
    {
      if (dump_file)
	{
	  fprintf (dump_file, "\nUnsafe conditional statement ");
	  print_gimple_stmt (dump_file, acc->stmt, 0, 0);
	}
      *(bool *) data = false;
      return 0;
    }
  return 1;
}

/* This function excludes statements that are part of allocation sites and
   field accesses from the hashtable of general accesses of the structure
   type STR. Only accesses that belong to the function represented by
   NODE are treated.  */

static void
exclude_alloc_and_field_accs_1 (d_str str, struct cgraph_node *node)
{
  struct exclude_data dt;
  dt.str = str;
  dt.fn_decl = node->decl;

  if (dt.str->accs)
    htab_traverse (dt.str->accs, exclude_from_accs, &dt);
}

/* Collect accesses to the structure types that appear in basic block BB.  */

static void
collect_accesses_in_bb (basic_block bb)
{
  gimple_stmt_iterator bsi;
  struct walk_stmt_info wi;

  memset (&wi, 0, sizeof (wi));

  for (bsi = gsi_start_bb (bb); !gsi_end_p (bsi); gsi_next (&bsi))
    {
      gimple stmt = gsi_stmt (bsi);

      /* In asm stmt we cannot always track the arguments,
	 so we just give up.  */
      if (gimple_code (stmt) == GIMPLE_ASM)
	{
	  free_structures ();
	  break;
	}

      wi.info = (void *) stmt;
      walk_gimple_op (stmt, get_stmt_accesses, &wi);
    }
}

/* This function generates cluster substructure that contains FIELDS.
   The cluster added to the set of clusters of the structure STR.  */

static void
gen_cluster (sbitmap fields, d_str str)
{
  struct field_cluster *crr_cluster = XCNEW (struct field_cluster);

  crr_cluster->sibling = str->struct_clustering;
  str->struct_clustering = crr_cluster;
  crr_cluster->fields_in_cluster = fields;
}

/* This function peels a field with the index I from the structure DS.  */

static void
peel_field (int i, d_str ds)
{
  struct field_cluster *crr_cluster = XCNEW (struct field_cluster);

  crr_cluster->sibling = ds->struct_clustering;
  ds->struct_clustering = crr_cluster;
  crr_cluster->fields_in_cluster =
    sbitmap_alloc ((unsigned int) ds->num_fields);
  sbitmap_zero (crr_cluster->fields_in_cluster);
  SET_BIT (crr_cluster->fields_in_cluster, i);
}

/* This function calculates maximum field count in
   the structure STR.  */

static gcov_type
get_max_field_count (d_str str)
{
  gcov_type max = 0;
  int i;

  for (i = 0; i < str->num_fields; i++)
    if (str->fields[i].count > max)
      max = str->fields[i].count;

  return max;
}

/* Do struct-reorg transformation for individual function
   represented by NODE. All structure types relevant
   for this function are transformed.  */

static void
do_reorg_for_func (struct cgraph_node *node)
{
  create_new_local_vars ();
  create_new_alloc_sites_for_func (node);
  create_new_accesses_for_func ();
  update_ssa (TODO_update_ssa);
  cleanup_tree_cfg ();

  /* Free auxiliary data representing local variables.  */
  free_new_vars_htab (new_local_vars);
}

/* Print structure TYPE, its name, if it exists, and body.
   INDENT defines the level of indentation (similar
   to the option -i of indent command). SHIFT parameter
   defines a number of spaces by which a structure will
   be shifted right.  */

static void
dump_struct_type (tree type, unsigned HOST_WIDE_INT indent,
		   unsigned HOST_WIDE_INT shift)
{
  const char *struct_name;
  tree field;

  if (!type || !dump_file)
    return;

  if (TREE_CODE (type) != RECORD_TYPE)
    {
      print_generic_expr (dump_file, type, 0);
      return;
    }

  print_shift (shift);
  struct_name = get_type_name (type);
  fprintf (dump_file, "struct ");
  if (struct_name)
    fprintf (dump_file, "%s\n",struct_name);
  print_shift (shift);
  fprintf (dump_file, "{\n");

  for (field = TYPE_FIELDS (type); field;
       field = TREE_CHAIN (field))
    {
      unsigned HOST_WIDE_INT s = indent;
      tree f_type = TREE_TYPE (field);

      print_shift (shift);
      while (s--)
	fprintf (dump_file, " ");
      dump_struct_type (f_type, indent, shift + indent);
      fprintf(dump_file, " ");
      print_generic_expr (dump_file, field, 0);
      fprintf(dump_file, ";\n");
    }
  print_shift (shift);
  fprintf (dump_file, "}\n");
}

/* This function creates new structure types to replace original type,
   indicated by STR->decl. The names of the new structure types are
   derived from the original structure type. If the original structure
   type has no name, we assume that its name is 'struct.<STR_NUM>'.  */

static void
create_new_type (d_str str, int *str_num)
{
  int cluster_num = 0;

  struct field_cluster *cluster = str->struct_clustering;
  while (cluster)
    {
      tree  name = gen_cluster_name (str->decl, cluster_num,
				     *str_num);
      tree fields;
      tree new_type;
      cluster_num++;

      fields = create_fields (cluster, str->fields,
			      str->num_fields);
      new_type = build_basic_struct (fields, name, str->decl);

      update_fields_mapping (cluster, new_type,
			     str->fields, str->num_fields);

      VEC_safe_push (tree, heap, str->new_types, new_type);
      cluster = cluster->sibling;
    }
  (*str_num)++;
}

/* This function is a callback for alloc_sites hashtable
   traversal. SLOT is a pointer to fallocs_t.
   This function frees memory pointed by *SLOT.  */

static int
free_falloc_sites (void **slot, void *data ATTRIBUTE_UNUSED)
{
  fallocs_t fallocs = *(fallocs_t *) slot;

  VEC_free (alloc_site_t, heap, fallocs->allocs);
  free (fallocs);
  return 1;
}

/* Remove structures collected in UNSUITABLE_TYPES
   from structures vector.  */

static void
remove_unsuitable_types (VEC (tree, heap) *unsuitable_types)
{
  d_str str;
  tree type;
  unsigned i, j;

  for (j = 0; VEC_iterate (tree, unsuitable_types, j, type); j++)
    for (i = 0; VEC_iterate (structure, structures, i, str); i++)
      if (is_equal_types (str->decl, type))
	{
	  remove_structure (i);
	  break;
	}
}

/* Exclude structure types with bitfields.
   We would not want to interfere with other optimizations
   that can be done in this case. The structure types with
   bitfields are added to UNSUITABLE_TYPES vector.  */

static void
exclude_types_with_bit_fields (VEC (tree, heap) **unsuitable_types)
{
  d_str str;
  unsigned i;

  for (i = 0; VEC_iterate (structure, structures, i, str); i++)
    check_bitfields (str, unsuitable_types);
}

/* This function checks three types of escape. A structure type escapes:

   1. if it's a type of parameter of a local function.
   2. if it's a type of function return value.
   3. if it escapes as a result of ipa-type-escape analysis.

  The escaping structure types are added to UNSUITABLE_TYPES vector.  */

static void
exclude_escaping_types (VEC (tree, heap) **unsuitable_types)
{
  exclude_types_passed_to_local_func (unsuitable_types);
  exclude_returned_types (unsuitable_types);
  exclude_escaping_types_1 (unsuitable_types);
}

/* This function analyzes whether the form of
   structure is such that we are capable to transform it.
   Nested structures are checked here. Unsuitable structure
   types are added to UNSUITABLE_TYPE vector.  */

static void
analyze_struct_form (VEC (tree, heap) **unsuitable_types)
{
  d_str str;
  unsigned i;

  for (i = 0; VEC_iterate (structure, structures, i, str); i++)
    check_struct_form (str, unsuitable_types);
}

/* This function looks for structure types instantiated in the program.
   The candidate types are added to the structures vector.
   Unsuitable types are collected into UNSUITABLE_TYPES vector.  */

static void
build_data_structure (VEC (tree, heap) **unsuitable_types)
{
  tree var, type;
  tree var_list;
  struct varpool_node *current_varpool;
  struct cgraph_node *c_node;

  /* Check global variables.  */
  FOR_EACH_STATIC_VARIABLE (current_varpool)
    {
      var = current_varpool->decl;
      if (is_candidate (var, &type, unsuitable_types))
	add_structure (type);
    }

  /* Now add structures that are types of function parameters and
     local variables.  */
  for (c_node = cgraph_nodes; c_node; c_node = c_node->next)
    {
      enum availability avail =
	cgraph_function_body_availability (c_node);

      /* We need AVAIL_AVAILABLE for main function.  */
      if (avail == AVAIL_LOCAL || avail == AVAIL_AVAILABLE)
	{
	  struct function *fn = DECL_STRUCT_FUNCTION (c_node->decl);

	  for (var = DECL_ARGUMENTS (c_node->decl); var;
	       var = TREE_CHAIN (var))
	      if (is_candidate (var, &type, unsuitable_types))
		add_structure (type);

	  if (fn == NULL)
	    {
	      /* Skip cones that haven't been materialized yet.  */
	      gcc_assert (c_node->clone_of
			  && c_node->clone_of->decl != c_node->decl);
	      continue;
	    }

	  /* Check function local variables.  */
	  for (var_list = fn->local_decls; var_list;
	       var_list = TREE_CHAIN (var_list))
	    {
	      var = TREE_VALUE (var_list);

	      if (is_candidate (var, &type, unsuitable_types))
		add_structure (type);
	    }
	}
    }
}

/* This function returns true if the program contains
   a call to user defined allocation function, or other
   functions that can interfere with struct-reorg optimizations.  */

static bool
program_redefines_malloc_p (void)
{
  struct cgraph_node *c_node;
  struct cgraph_node *c_node2;
  struct cgraph_edge *c_edge;
  tree fndecl2;

  for (c_node = cgraph_nodes; c_node; c_node = c_node->next)
    {
      for (c_edge = c_node->callees; c_edge; c_edge = c_edge->next_callee)
	{
	  c_node2 = c_edge->callee;
	  fndecl2 = c_node2->decl;
	  if (is_gimple_call (c_edge->call_stmt))
	    {
	      const char * fname = get_name (fndecl2);

	      if ((gimple_call_flags (c_edge->call_stmt) & ECF_MALLOC)
		  && (DECL_FUNCTION_CODE (fndecl2) != BUILT_IN_MALLOC)
		  && (DECL_FUNCTION_CODE (fndecl2) != BUILT_IN_CALLOC)
		  && (DECL_FUNCTION_CODE (fndecl2) != BUILT_IN_ALLOCA))
		return true;

	      /* Check that there is no __builtin_object_size,
	       __builtin_offsetof, or realloc's in the program.  */
	      if (DECL_FUNCTION_CODE (fndecl2) == BUILT_IN_OBJECT_SIZE
		  || !strcmp (fname, "__builtin_offsetof")
		  || !strcmp (fname, "realloc"))
		return true;
	    }
	}
    }

  return false;
}

/* In this function we assume that an allocation statement

   var = (type_cast) malloc (size);

   is converted into the following set of statements:

   T.1 = size;
   T.2 = malloc (T.1);
   T.3 = (type_cast) T.2;
   var = T.3;

   In this function we collect into alloc_sites the allocation
   sites of variables of structure types that are present
   in structures vector.  */

static void
collect_alloc_sites (void)
{
  struct cgraph_node *node;
  struct cgraph_edge *cs;

  for (node = cgraph_nodes; node; node = node->next)
    if (node->analyzed && node->decl)
      {
	for (cs = node->callees; cs; cs = cs->next_callee)
	  {
	    gimple stmt = cs->call_stmt;

	    if (stmt)
	      {
		tree decl;

		if (is_gimple_call (stmt)
		    && (decl = gimple_call_fndecl (stmt))
		    && gimple_call_lhs (stmt))
		  {
		    unsigned i;

		    if (is_alloc_of_struct (stmt, &i))
		      {
			/* We support only malloc now.  */
			if (DECL_FUNCTION_CODE (decl) == BUILT_IN_MALLOC)
			  {
			    d_str str;

			    str = VEC_index (structure, structures, i);
			    add_alloc_site (node->decl, stmt, str);
			  }
			else
			  {
			    if (dump_file)
			      {
				fprintf (dump_file,
					 "\nUnsupported allocation function ");
				print_gimple_stmt (dump_file, stmt, 0, 0);
			      }
			    remove_structure (i);
			  }
		      }
		  }
	      }
	  }
      }
}

/* Print collected accesses.  */

static void
dump_accesses (void)
{
  d_str str;
  unsigned i;

  if (!dump_file)
    return;

  for (i = 0; VEC_iterate (structure, structures, i, str); i++)
    dump_accs (str);
}

/* This function checks whether the accesses of structures in condition
   expressions are of the kind we are capable to transform.
   If not, such structures are removed from the vector of structures.  */

static void
check_cond_exprs (void)
{
  d_str str;
  unsigned i;

  i = 0;
  while (VEC_iterate (structure, structures, i, str))
    {
      bool safe_p = true;

      if (str->accs)
	htab_traverse (str->accs, safe_cond_expr_check, &safe_p);
      if (!safe_p)
	remove_structure (i);
      else
	i++;
    }
}

/* We exclude from non-field accesses of the structure
   all statements that will be treated as part of the structure
   allocation sites or field accesses.  */

static void
exclude_alloc_and_field_accs (struct cgraph_node *node)
{
  d_str str;
  unsigned i;

  for (i = 0; VEC_iterate (structure, structures, i, str); i++)
    exclude_alloc_and_field_accs_1 (str, node);
}

/* This function collects accesses of the fields of the structures
   that appear at function FN.  */

static void
collect_accesses_in_func (struct function *fn)
{
  basic_block bb;

  if (! fn)
    return;

  /* Collect accesses for each basic blocks separately.  */
  FOR_EACH_BB_FN (bb, fn)
    collect_accesses_in_bb (bb);
}

/* This function summarizes counts of the fields into the structure count.  */

static void
sum_counts (d_str str, gcov_type *hottest)
{
  int i;

  str->count = 0;
  for (i = 0; i < str->num_fields; i++)
    {
      if (dump_file)
	{
	  fprintf (dump_file, "\nCounter of field \"");
	  print_generic_expr (dump_file, str->fields[i].decl, 0);
	  fprintf (dump_file, "\" is " HOST_WIDEST_INT_PRINT_DEC,
		   str->fields[i].count);
	}
      str->count += str->fields[i].count;
    }

  if (dump_file)
    {
      fprintf (dump_file, "\nCounter of struct \"");
      print_generic_expr (dump_file, str->decl, 0);
      fprintf (dump_file, "\" is " HOST_WIDEST_INT_PRINT_DEC, str->count);
    }

  if (str->count > *hottest)
    *hottest = str->count;
}

/* This function peels the field into separate structure if it's
   sufficiently hot, i.e. if its count provides at least 90% of
   the maximum field count in the structure.  */

static void
peel_hot_fields (d_str str)
{
  gcov_type max_field_count;
  sbitmap fields_left = sbitmap_alloc (str->num_fields);
  int i;

  sbitmap_ones (fields_left);
  max_field_count =
    (gcov_type) (get_max_field_count (str)/100)*90;

  str->struct_clustering = NULL;

  for (i = 0; i < str->num_fields; i++)
    {
      if (str->count >= max_field_count)
	{
	  RESET_BIT (fields_left, i);
	  peel_field (i, str);
	}
    }

  i = sbitmap_first_set_bit (fields_left);
  if (i != -1)
    gen_cluster (fields_left, str);
  else
    sbitmap_free (fields_left);
}

/* This function is a helper for do_reorg. It goes over
   functions in call graph and performs actual transformation
   on them.  */

static void
do_reorg_1 (void)
{
  struct cgraph_node *node;

  /* Initialize the default bitmap obstack.  */
  bitmap_obstack_initialize (NULL);

  for (node = cgraph_nodes; node; node = node->next)
    if (node->analyzed && node->decl)
      {
	push_cfun (DECL_STRUCT_FUNCTION (node->decl));
	current_function_decl = node->decl;
	if (dump_file)
	  fprintf (dump_file, "\nFunction to do reorg is %s: \n",
		   (const char *) IDENTIFIER_POINTER (DECL_NAME (node->decl)));
	do_reorg_for_func (node);
	free_dominance_info (CDI_DOMINATORS);
	free_dominance_info (CDI_POST_DOMINATORS);
	current_function_decl = NULL;
	pop_cfun ();
      }

  set_cfun (NULL);
  bitmap_obstack_release (NULL);
}

/* This function creates new global struct variables.
   For each original variable, the set of new variables
   is created with the new structure types corresponding
   to the structure type of original variable.
   Only VAR_DECL variables are treated by this function.  */

static void
create_new_global_vars (void)
{
  struct varpool_node *current_varpool;
  unsigned HOST_WIDE_INT i;
  unsigned HOST_WIDE_INT varpool_size = 0;

  for (i = 0; i < 2; i++)
    {
      if (i)
	new_global_vars = htab_create (varpool_size,
				       new_var_hash, new_var_eq, NULL);
      FOR_EACH_STATIC_VARIABLE(current_varpool)
	{
	  tree  var_decl = current_varpool->decl;

	  if (!var_decl || TREE_CODE (var_decl) != VAR_DECL)
	    continue;
	  if (!i)
	    varpool_size++;
	  else
	    create_new_var (var_decl, new_global_vars);
	}
    }

  if (new_global_vars)
    htab_traverse (new_global_vars, update_varpool_with_new_var, NULL);
}

/* Dump all new types generated by this optimization.  */

static void
dump_new_types (void)
{
  d_str str;
  tree type;
  unsigned i, j;

  if (!dump_file)
    return;

  fprintf (dump_file, "\nThe following are the new types generated by"
	   " this optimization:\n");

  for (i = 0; VEC_iterate (structure, structures, i, str); i++)
    {
      if (dump_file)
	{
	  fprintf (dump_file, "\nFor type ");
	  dump_struct_type (str->decl, 2, 0);
	  fprintf (dump_file, "\nthe number of new types is %d\n",
		   VEC_length (tree, str->new_types));
	}
      for (j = 0; VEC_iterate (tree, str->new_types, j, type); j++)
	dump_struct_type (type, 2, 0);
    }
}

/* This function creates new types to replace old structure types.  */

static void
create_new_types (void)
{
  d_str str;
  unsigned i;
  int str_num = 0;

  for (i = 0; VEC_iterate (structure, structures, i, str); i++)
    create_new_type (str, &str_num);
}

/* Free allocation sites hashtable.  */

static void
free_alloc_sites (void)
{
  if (alloc_sites)
    htab_traverse (alloc_sites, free_falloc_sites, NULL);
  htab_delete (alloc_sites);
  alloc_sites = NULL;
}

/* This function collects structures potential
   for peeling transformation, and inserts
   them into structures hashtable.  */

static void
collect_structures (void)
{
  VEC (tree, heap) *unsuitable_types = VEC_alloc (tree, heap, 32);

  structures = VEC_alloc (structure, heap, 32);

  /* If program contains user defined mallocs, we give up.  */
  if (program_redefines_malloc_p ())
     return;

  /* Build data structures hashtable of all data structures
     in the program.  */
  build_data_structure (&unsuitable_types);

  /* This function analyzes whether the form of
     structure is such that we are capable to transform it.
     Nested structures are checked here.  */
  analyze_struct_form (&unsuitable_types);

  /* This function excludes those structure types
     that escape compilation unit.  */
  exclude_escaping_types (&unsuitable_types);

  /* We do not want to change data layout of the structures with bitfields.  */
  exclude_types_with_bit_fields (&unsuitable_types);

  remove_unsuitable_types (unsuitable_types);
  VEC_free (tree, heap, unsuitable_types);
}

/* Collect structure allocation sites. In case of arrays
   we have nothing to do.  */

static void
collect_allocation_sites (void)
{
  alloc_sites = htab_create (32, malloc_hash, malloc_eq, NULL);
  collect_alloc_sites ();
}

/* This function collects data accesses for the
   structures to be transformed. For each structure
   field it updates the count field in field_entry.  */

static void
collect_data_accesses (void)
{
  struct cgraph_node *c_node;

  for (c_node = cgraph_nodes; c_node; c_node = c_node->next)
    {
      enum availability avail = cgraph_function_body_availability (c_node);

      if (avail == AVAIL_LOCAL || avail == AVAIL_AVAILABLE)
	{
	  struct function *func = DECL_STRUCT_FUNCTION (c_node->decl);

	  collect_accesses_in_func (func);
	  exclude_alloc_and_field_accs (c_node);
	}
    }

  check_cond_exprs ();
  /* Print collected accesses.  */
  dump_accesses ();
}

/* We do not bother to transform cold structures.
   Coldness of the structure is defined relatively
   to the highest structure count among the structures
   to be transformed. It's triggered by the compiler parameter

   --param struct-reorg-cold-struct-ratio=<value>

   where <value> ranges from 0 to 100. Structures with count ratios
   that are less than this parameter are considered to be cold.  */

static void
exclude_cold_structs (void)
{
  gcov_type hottest = 0;
  unsigned i;
  d_str str;

  /* We summarize counts of fields of a structure into the structure count.  */
  for (i = 0; VEC_iterate (structure, structures, i, str); i++)
    sum_counts (str, &hottest);

  /* Remove cold structures from structures vector.  */
  i = 0;
  while (VEC_iterate (structure, structures, i, str))
    if (str->count * 100 < (hottest * STRUCT_REORG_COLD_STRUCT_RATIO))
      {
	if (dump_file)
	  {
	    fprintf (dump_file, "\nThe structure ");
	    print_generic_expr (dump_file, str->decl, 0);
	    fprintf (dump_file, " is cold.");
	  }
	remove_structure (i);
      }
    else
      i++;
}

/* This function decomposes original structure into substructures,
   i.e.clusters.  */

static void
peel_structs (void)
{
  d_str str;
  unsigned i;

  for (i = 0; VEC_iterate (structure, structures, i, str); i++)
    peel_hot_fields (str);
}

/* Stage 3.  */
/* Do the actual transformation for each structure
   from the structures hashtable.  */

static void
do_reorg (void)
{
  /* Check that there is a work to do.  */
  if (!VEC_length (structure, structures))
    {
      if (dump_file)
	fprintf (dump_file, "\nNo structures to transform. Exiting...");
      return;
    }
  else
    {
      if (dump_file)
	{
	  fprintf (dump_file, "\nNumber of structures to transform is %d",
		   VEC_length (structure, structures));
	}
    }

  /* Generate new types.  */
  create_new_types ();
  dump_new_types ();

  /* Create new global variables.  */
  create_new_global_vars ();
  dump_new_vars (new_global_vars);

  /* Decompose structures for each function separately.  */
  do_reorg_1 ();

  /* Free auxiliary data collected for global variables.  */
  free_new_vars_htab (new_global_vars);
}

/* Free all auxiliary data used by this optimization.  */

static void
free_data_structs (void)
{
  free_structures ();
  free_alloc_sites ();
}

/* Perform structure decomposition (peeling).  */

static void
reorg_structs (void)
{

  /* Stage1.  */
  /* Collect structure types.  */
  collect_structures ();

  /* Collect structure allocation sites.  */
  collect_allocation_sites ();

  /* Collect structure accesses.  */
  collect_data_accesses ();

  /* We transform only hot structures.  */
  exclude_cold_structs ();

  /* Stage2.  */
  /* Decompose structures into substructures, i.e. clusters.  */
  peel_structs ();

  /* Stage3. */
  /* Do the actual transformation for each structure
     from the structures hashtable.  */
  do_reorg ();

  /* Free all auxiliary data used by this optimization.  */
  free_data_structs ();
}

/* Struct-reorg optimization entry point function.  */

static unsigned int
reorg_structs_drive (void)
{
  reorg_structs ();
  return 0;
}

/* Struct-reorg optimization gate function.  */

static bool
struct_reorg_gate (void)
{
  return flag_ipa_struct_reorg
	 && flag_whole_program
	 && (optimize > 0);
}

struct simple_ipa_opt_pass pass_ipa_struct_reorg =
{
 {
  SIMPLE_IPA_PASS,
  "ipa_struct_reorg",	 	  /* name */
  struct_reorg_gate,		  /* gate */
  reorg_structs_drive,		  /* execute */
  NULL,				  /* sub */
  NULL,				  /* next */
  0,				  /* static_pass_number */
  TV_INTEGRATION,		  /* tv_id */
  0,	                          /* properties_required */
  0,				  /* properties_provided */
  0,				  /* properties_destroyed */
  TODO_verify_ssa,		  /* todo_flags_start */
  TODO_dump_func | TODO_verify_ssa	/* todo_flags_finish */
 }
};
