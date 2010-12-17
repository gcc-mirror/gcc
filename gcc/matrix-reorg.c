/* Matrix layout transformations.
   Copyright (C) 2006, 2007, 2008, 2009, 2010 Free Software Foundation, Inc.
   Contributed by Razya Ladelsky <razya@il.ibm.com>
   Originally written by Revital Eres and Mustafa Hagog.

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

/*
   Matrix flattening optimization tries to replace a N-dimensional
   matrix with its equivalent M-dimensional matrix, where M < N.
   This first implementation focuses on global matrices defined dynamically.

   When N==1, we actually flatten the whole matrix.
   For instance consider a two-dimensional array a [dim1] [dim2].
   The code for allocating space for it usually looks like:

     a = (int **)  malloc(dim1 * sizeof(int *));
     for (i=0; i<dim1; i++)
        a[i] = (int *) malloc (dim2 * sizeof(int));

   If the array "a" is found suitable for this optimization,
   its allocation is replaced by:

     a = (int *) malloc (dim1 * dim2 *sizeof(int));

   and all the references to a[i][j] are replaced by a[i * dim2 + j].

   The two main phases of the optimization are the analysis
   and transformation.
   The driver of the optimization is matrix_reorg ().



   Analysis phase:
   ===============

   We'll number the dimensions outside-in, meaning the most external
   is 0, then 1, and so on.
   The analysis part of the optimization determines K, the escape
   level of a N-dimensional matrix (K <= N), that allows flattening of
   the external dimensions 0,1,..., K-1. Escape level 0 means that the
   whole matrix escapes and no flattening is possible.

   The analysis part is implemented in analyze_matrix_allocation_site()
   and analyze_matrix_accesses().

   Transformation phase:
   =====================
   In this phase we define the new flattened matrices that replace the
   original matrices in the code.
   Implemented in transform_allocation_sites(),
   transform_access_sites().

   Matrix Transposing
   ==================
   The idea of Matrix Transposing is organizing the matrix in a different
   layout such that the dimensions are reordered.
   This could produce better cache behavior in some cases.

   For example, lets look at the matrix accesses in the following loop:

   for (i=0; i<N; i++)
    for (j=0; j<M; j++)
     access to a[i][j]

   This loop can produce good cache behavior because the elements of
   the inner dimension are accessed sequentially.

  However, if the accesses of the matrix were of the following form:

  for (i=0; i<N; i++)
   for (j=0; j<M; j++)
     access to a[j][i]

  In this loop we iterate the columns and not the rows.
  Therefore, replacing the rows and columns
  would have had an organization with better (cache) locality.
  Replacing the dimensions of the matrix is called matrix transposing.

  This  example, of course, could be enhanced to multiple dimensions matrices
  as well.

  Since a program could include all kind of accesses, there is a decision
  mechanism, implemented in analyze_transpose(), which implements a
  heuristic that tries to determine whether to transpose the matrix or not,
  according to the form of the more dominant accesses.
  This decision is transferred to the flattening mechanism, and whether
  the matrix was transposed or not, the matrix is flattened (if possible).

  This decision making is based on profiling information and loop information.
  If profiling information is available, decision making mechanism will be
  operated, otherwise the matrix will only be flattened (if possible).

  Both optimizations are described in the paper "Matrix flattening and
  transposing in GCC" which was presented in GCC summit 2006.
  http://www.gccsummit.org/2006/2006-GCC-Summit-Proceedings.pdf.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "tree.h"
#include "rtl.h"
#include "tree-inline.h"
#include "tree-flow.h"
#include "tree-flow-inline.h"
#include "langhooks.h"
#include "hashtab.h"
#include "flags.h"
#include "ggc.h"
#include "debug.h"
#include "target.h"
#include "cgraph.h"
#include "diagnostic-core.h"
#include "timevar.h"
#include "params.h"
#include "fibheap.h"
#include "intl.h"
#include "function.h"
#include "basic-block.h"
#include "cfgloop.h"
#include "tree-iterator.h"
#include "tree-pass.h"
#include "opts.h"
#include "tree-data-ref.h"
#include "tree-chrec.h"
#include "tree-scalar-evolution.h"
#include "tree-ssa-sccvn.h"

/* We need to collect a lot of data from the original malloc,
   particularly as the gimplifier has converted:

   orig_var = (struct_type *) malloc (x * sizeof (struct_type *));

   into

   T3 = <constant> ;  ** <constant> is amount to malloc; precomputed **
   T4 = malloc (T3);
   T5 = (struct_type *) T4;
   orig_var = T5;

   The following struct fields allow us to collect all the necessary data from
   the gimplified program.  The comments in the struct below are all based
   on the gimple example above.  */

struct malloc_call_data
{
  gimple call_stmt;		/* Tree for "T4 = malloc (T3);"                     */
  tree size_var;		/* Var decl for T3.                                 */
  tree malloc_size;		/* Tree for "<constant>", the rhs assigned to T3.   */
};

static tree can_calculate_expr_before_stmt (tree, sbitmap);
static tree can_calculate_stmt_before_stmt (gimple, sbitmap);

/* The front end of the compiler, when parsing statements of the form:

   var = (type_cast) malloc (sizeof (type));

   always converts this single statement into the following statements
   (GIMPLE form):

   T.1 = sizeof (type);
   T.2 = malloc (T.1);
   T.3 = (type_cast) T.2;
   var = T.3;

   Since we need to create new malloc statements and modify the original
   statements somewhat, we need to find all four of the above statements.
   Currently record_call_1 (called for building cgraph edges) finds and
   records the statements containing the actual call to malloc, but we
   need to find the rest of the variables/statements on our own.  That
   is what the following function does.  */
static void
collect_data_for_malloc_call (gimple stmt, struct malloc_call_data *m_data)
{
  tree size_var = NULL;
  tree malloc_fn_decl;
  tree arg1;

  gcc_assert (is_gimple_call (stmt));

  malloc_fn_decl = gimple_call_fndecl (stmt);
  if (malloc_fn_decl == NULL
      || DECL_FUNCTION_CODE (malloc_fn_decl) != BUILT_IN_MALLOC)
    return;

  arg1 = gimple_call_arg (stmt, 0);
  size_var = arg1;

  m_data->call_stmt = stmt;
  m_data->size_var = size_var;
  if (TREE_CODE (size_var) != VAR_DECL)
    m_data->malloc_size = size_var;
  else
    m_data->malloc_size = NULL_TREE;
}

/* Information about matrix access site.
   For example: if an access site of matrix arr is arr[i][j]
   the ACCESS_SITE_INFO structure will have the address
   of arr as its stmt.  The INDEX_INFO will hold information about the
   initial address and index of each dimension.  */
struct access_site_info
{
  /* The statement (MEM_REF or POINTER_PLUS_EXPR).  */
  gimple stmt;

  /* In case of POINTER_PLUS_EXPR, what is the offset.  */
  tree offset;

  /* The index which created the offset.  */
  tree index;

  /* The indirection level of this statement.  */
  int level;

  /* TRUE for allocation site FALSE for access site.  */
  bool is_alloc;

  /* The function containing the access site.  */
  tree function_decl;

  /* This access is iterated in the inner most loop */
  bool iterated_by_inner_most_loop_p;
};

typedef struct access_site_info *access_site_info_p;
DEF_VEC_P (access_site_info_p);
DEF_VEC_ALLOC_P (access_site_info_p, heap);

/* Calls to free when flattening a matrix.  */

struct free_info
{
  gimple stmt;
  tree func;
};

/* Information about matrix to flatten.  */
struct matrix_info
{
  /* Decl tree of this matrix.  */
  tree decl;
  /* Number of dimensions; number
     of "*" in the type declaration.  */
  int num_dims;

  /* Minimum indirection level that escapes, 0 means that
     the whole matrix escapes, k means that dimensions
     0 to ACTUAL_DIM - k escapes.  */
  int min_indirect_level_escape;

  gimple min_indirect_level_escape_stmt;

  /* Hold the allocation site for each level (dimension).
     We can use NUM_DIMS as the upper bound and allocate the array
     once with this number of elements and no need to use realloc and
     MAX_MALLOCED_LEVEL.  */
  gimple *malloc_for_level;

  int max_malloced_level;

  /* Is the matrix transposed.  */
  bool is_transposed_p;

  /* The location of the allocation sites (they must be in one
     function).  */
  tree allocation_function_decl;

  /* The calls to free for each level of indirection.  */
  struct free_info *free_stmts;

  /* An array which holds for each dimension its size. where
     dimension 0 is the outer most (one that contains all the others).
   */
  tree *dimension_size;

  /* An array which holds for each dimension it's original size
     (before transposing and flattening take place).  */
  tree *dimension_size_orig;

  /* An array which holds for each dimension the size of the type of
     of elements accessed in that level (in bytes).  */
  HOST_WIDE_INT *dimension_type_size;

  int dimension_type_size_len;

  /* An array collecting the count of accesses for each dimension.  */
  gcov_type *dim_hot_level;

  /* An array of the accesses to be flattened.
     elements are of type "struct access_site_info *".  */
  VEC (access_site_info_p, heap) * access_l;

  /* A map of how the dimensions will be organized at the end of
     the analyses.  */
  int *dim_map;
};

/* In each phi node we want to record the indirection level we have when we
   get to the phi node.  Usually we will have phi nodes with more than two
   arguments, then we must assure that all of them get to the phi node with
   the same indirection level, otherwise it's not safe to do the flattening.
   So we record the information regarding the indirection level each time we
   get to the phi node in this hash table.  */

struct matrix_access_phi_node
{
  gimple phi;
  int indirection_level;
};

/* We use this structure to find if the SSA variable is accessed inside the
   tree and record the tree containing it.  */

struct ssa_acc_in_tree
{
  /* The variable whose accesses in the tree we are looking for.  */
  tree ssa_var;
  /* The tree and code inside it the ssa_var is accessed, currently
     it could be an MEM_REF or CALL_EXPR.  */
  enum tree_code t_code;
  tree t_tree;
  /* The place in the containing tree.  */
  tree *tp;
  tree second_op;
  bool var_found;
};

static void analyze_matrix_accesses (struct matrix_info *, tree, int, bool,
				     sbitmap, bool);
static int transform_allocation_sites (void **, void *);
static int transform_access_sites (void **, void *);
static int analyze_transpose (void **, void *);
static int dump_matrix_reorg_analysis (void **, void *);

static bool check_transpose_p;

/* Hash function used for the phi nodes.  */

static hashval_t
mat_acc_phi_hash (const void *p)
{
  const struct matrix_access_phi_node *const ma_phi =
    (const struct matrix_access_phi_node *) p;

  return htab_hash_pointer (ma_phi->phi);
}

/* Equality means phi node pointers are the same.  */

static int
mat_acc_phi_eq (const void *p1, const void *p2)
{
  const struct matrix_access_phi_node *const phi1 =
    (const struct matrix_access_phi_node *) p1;
  const struct matrix_access_phi_node *const phi2 =
    (const struct matrix_access_phi_node *) p2;

  if (phi1->phi == phi2->phi)
    return 1;

  return 0;
}

/* Hold the PHI nodes we visit during the traversal for escaping
   analysis.  */
static htab_t htab_mat_acc_phi_nodes = NULL;

/* This hash-table holds the information about the matrices we are
   going to handle.  */
static htab_t matrices_to_reorg = NULL;

/* Return a hash for MTT, which is really a "matrix_info *".  */
static hashval_t
mtt_info_hash (const void *mtt)
{
  return htab_hash_pointer (((const struct matrix_info *) mtt)->decl);
}

/* Return true if MTT1 and MTT2 (which are really both of type
   "matrix_info *") refer to the same decl.  */
static int
mtt_info_eq (const void *mtt1, const void *mtt2)
{
  const struct matrix_info *const i1 = (const struct matrix_info *) mtt1;
  const struct matrix_info *const i2 = (const struct matrix_info *) mtt2;

  if (i1->decl == i2->decl)
    return true;

  return false;
}

/* Return false if STMT may contain a vector expression.
   In this situation, all matrices should not be flattened.  */
static bool
may_flatten_matrices_1 (gimple stmt)
{
  switch (gimple_code (stmt))
    {
    case GIMPLE_ASSIGN:
    case GIMPLE_CALL:
      if (!gimple_has_lhs (stmt))
	return true;
      if (TREE_CODE (TREE_TYPE (gimple_get_lhs (stmt))) == VECTOR_TYPE)
	{
	  if (dump_file)
	    fprintf (dump_file,
		     "Found vector type, don't flatten matrix\n");
	  return false;
	}
      break;
    case GIMPLE_ASM:
      /* Asm code could contain vector operations.  */
      return false;
      break;
    default:
      break;
    }
  return true;
}

/* Return false if there are hand-written vectors in the program.
   We disable the flattening in such a case.  */
static bool
may_flatten_matrices (struct cgraph_node *node)
{
  tree decl;
  struct function *func;
  basic_block bb;
  gimple_stmt_iterator gsi;

  decl = node->decl;
  if (node->analyzed)
    {
      func = DECL_STRUCT_FUNCTION (decl);
      FOR_EACH_BB_FN (bb, func)
	for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
	if (!may_flatten_matrices_1 (gsi_stmt (gsi)))
	  return false;
    }
  return true;
}

/* Given a VAR_DECL, check its type to determine whether it is
   a definition of a dynamic allocated matrix and therefore is
   a suitable candidate for the matrix flattening optimization.
   Return NULL if VAR_DECL is not such decl.  Otherwise, allocate
   a MATRIX_INFO structure, fill it with the relevant information
   and return a pointer to it.
   TODO: handle also statically defined arrays.  */
static struct matrix_info *
analyze_matrix_decl (tree var_decl)
{
  struct matrix_info *m_node, tmpmi, *mi;
  tree var_type;
  int dim_num = 0;

  gcc_assert (matrices_to_reorg);

  if (TREE_CODE (var_decl) == PARM_DECL)
    var_type = DECL_ARG_TYPE (var_decl);
  else if (TREE_CODE (var_decl) == VAR_DECL)
    var_type = TREE_TYPE (var_decl);
  else
    return NULL;

  if (!POINTER_TYPE_P (var_type))
    return NULL;

  while (POINTER_TYPE_P (var_type))
    {
      var_type = TREE_TYPE (var_type);
      dim_num++;
    }

  if (dim_num <= 1)
    return NULL;

  if (!COMPLETE_TYPE_P (var_type)
      || TREE_CODE (TYPE_SIZE_UNIT (var_type)) != INTEGER_CST)
    return NULL;

  /* Check to see if this pointer is already in there.  */
  tmpmi.decl = var_decl;
  mi = (struct matrix_info *) htab_find (matrices_to_reorg, &tmpmi);

  if (mi)
    return NULL;

  /* Record the matrix.  */

  m_node = (struct matrix_info *) xcalloc (1, sizeof (struct matrix_info));
  m_node->decl = var_decl;
  m_node->num_dims = dim_num;
  m_node->free_stmts
    = (struct free_info *) xcalloc (dim_num, sizeof (struct free_info));

  /* Init min_indirect_level_escape to -1 to indicate that no escape
     analysis has been done yet.  */
  m_node->min_indirect_level_escape = -1;
  m_node->is_transposed_p = false;

  return m_node;
}

/* Free matrix E.  */
static void
mat_free (void *e)
{
  struct matrix_info *mat = (struct matrix_info *) e;

  if (!mat)
    return;

  if (mat->free_stmts)
    free (mat->free_stmts);
  if (mat->dim_hot_level)
    free (mat->dim_hot_level);
  if (mat->malloc_for_level)
    free (mat->malloc_for_level);
}

/* Find all potential matrices.
   TODO: currently we handle only multidimensional
   dynamically allocated arrays.  */
static void
find_matrices_decl (void)
{
  struct matrix_info *tmp;
  PTR *slot;
  struct varpool_node *vnode;

  gcc_assert (matrices_to_reorg);

  /* For every global variable in the program:
     Check to see if it's of a candidate type and record it.  */
  for (vnode = varpool_nodes_queue; vnode; vnode = vnode->next_needed)
    {
      tree var_decl = vnode->decl;

      if (!var_decl || TREE_CODE (var_decl) != VAR_DECL)
	continue;

      if (matrices_to_reorg)
	if ((tmp = analyze_matrix_decl (var_decl)))
	  {
	    if (!TREE_ADDRESSABLE (var_decl))
	      {
		slot = htab_find_slot (matrices_to_reorg, tmp, INSERT);
		*slot = tmp;
	      }
	  }
    }
  return;
}

/* Mark that the matrix MI escapes at level L.  */
static void
mark_min_matrix_escape_level (struct matrix_info *mi, int l, gimple s)
{
  if (mi->min_indirect_level_escape == -1
      || (mi->min_indirect_level_escape > l))
    {
      mi->min_indirect_level_escape = l;
      mi->min_indirect_level_escape_stmt = s;
    }
}

/* Find if the SSA variable is accessed inside the
   tree and record the tree containing it.
   The only relevant uses are the case of SSA_NAME, or SSA inside
   MEM_REF, PLUS_EXPR, POINTER_PLUS_EXPR, MULT_EXPR.  */
static void
ssa_accessed_in_tree (tree t, struct ssa_acc_in_tree *a)
{
  a->t_code = TREE_CODE (t);
  switch (a->t_code)
    {
    case SSA_NAME:
      if (t == a->ssa_var)
	a->var_found = true;
      break;
    case MEM_REF:
      if (SSA_VAR_P (TREE_OPERAND (t, 0))
	  && TREE_OPERAND (t, 0) == a->ssa_var)
	a->var_found = true;
      break;
    default:
      break;
    }
}

/* Find if the SSA variable is accessed on the right hand side of
   gimple call STMT. */

static void
ssa_accessed_in_call_rhs (gimple stmt, struct ssa_acc_in_tree *a)
{
  tree decl;
  tree arg;
  size_t i;

  a->t_code = CALL_EXPR;
  for (i = 0; i < gimple_call_num_args (stmt); i++)
    {
      arg = gimple_call_arg (stmt, i);
      if (arg == a->ssa_var)
	{
	  a->var_found = true;
	  decl = gimple_call_fndecl (stmt);
	  a->t_tree = decl;
	  break;
	}
    }
}

/* Find if the SSA variable is accessed on the right hand side of
   gimple assign STMT. */

static void
ssa_accessed_in_assign_rhs (gimple stmt, struct ssa_acc_in_tree *a)
{

  a->t_code = gimple_assign_rhs_code (stmt);
  switch (a->t_code)
    {
      tree op1, op2;

    case SSA_NAME:
    case MEM_REF:
    CASE_CONVERT:
    case VIEW_CONVERT_EXPR:
      ssa_accessed_in_tree (gimple_assign_rhs1 (stmt), a);
      break;
    case POINTER_PLUS_EXPR:
    case PLUS_EXPR:
    case MULT_EXPR:
      op1 = gimple_assign_rhs1 (stmt);
      op2 = gimple_assign_rhs2 (stmt);

      if (op1 == a->ssa_var)
	{
	  a->var_found = true;
	  a->second_op = op2;
	}
      else if (op2 == a->ssa_var)
	{
	  a->var_found = true;
	  a->second_op = op1;
	}
      break;
    default:
      break;
    }
}

/* Record the access/allocation site information for matrix MI so we can
   handle it later in transformation.  */
static void
record_access_alloc_site_info (struct matrix_info *mi, gimple stmt, tree offset,
			       tree index, int level, bool is_alloc)
{
  struct access_site_info *acc_info;

  if (!mi->access_l)
    mi->access_l = VEC_alloc (access_site_info_p, heap, 100);

  acc_info
    = (struct access_site_info *)
    xcalloc (1, sizeof (struct access_site_info));
  acc_info->stmt = stmt;
  acc_info->offset = offset;
  acc_info->index = index;
  acc_info->function_decl = current_function_decl;
  acc_info->level = level;
  acc_info->is_alloc = is_alloc;

  VEC_safe_push (access_site_info_p, heap, mi->access_l, acc_info);

}

/* Record the malloc as the allocation site of the given LEVEL.  But
   first we Make sure that all the size parameters passed to malloc in
   all the allocation sites could be pre-calculated before the call to
   the malloc of level 0 (the main malloc call).  */
static void
add_allocation_site (struct matrix_info *mi, gimple stmt, int level)
{
  struct malloc_call_data mcd;

  /* Make sure that the allocation sites are in the same function.  */
  if (!mi->allocation_function_decl)
    mi->allocation_function_decl = current_function_decl;
  else if (mi->allocation_function_decl != current_function_decl)
    {
      int min_malloc_level;

      gcc_assert (mi->malloc_for_level);

      /* Find the minimum malloc level that already has been seen;
         we known its allocation function must be
         MI->allocation_function_decl since it's different than
         CURRENT_FUNCTION_DECL then the escaping level should be
         MIN (LEVEL, MIN_MALLOC_LEVEL) - 1 , and the allocation function
         must be set accordingly.  */
      for (min_malloc_level = 0;
	   min_malloc_level < mi->max_malloced_level
	   && mi->malloc_for_level[min_malloc_level]; min_malloc_level++);
      if (level < min_malloc_level)
	{
	  mi->allocation_function_decl = current_function_decl;
	  mark_min_matrix_escape_level (mi, min_malloc_level, stmt);
	}
      else
	{
	  mark_min_matrix_escape_level (mi, level, stmt);
	  /* cannot be that (level == min_malloc_level)
	     we would have returned earlier.  */
	  return;
	}
    }

  /* Find the correct malloc information.  */
  collect_data_for_malloc_call (stmt, &mcd);

  /* We accept only calls to malloc function; we do not accept
     calls like calloc and realloc.  */
  if (!mi->malloc_for_level)
    {
      mi->malloc_for_level = XCNEWVEC (gimple, level + 1);
      mi->max_malloced_level = level + 1;
    }
  else if (mi->max_malloced_level <= level)
    {
      mi->malloc_for_level
	= XRESIZEVEC (gimple, mi->malloc_for_level, level + 1);

      /* Zero the newly allocated items.  */
      memset (&(mi->malloc_for_level[mi->max_malloced_level + 1]),
	      0, (level - mi->max_malloced_level) * sizeof (tree));

      mi->max_malloced_level = level + 1;
    }
  mi->malloc_for_level[level] = stmt;
}

/* Given an assignment statement STMT that we know that its
   left-hand-side is the matrix MI variable, we traverse the immediate
   uses backwards until we get to a malloc site.  We make sure that
   there is one and only one malloc site that sets this variable.  When
   we are performing the flattening we generate a new variable that
   will hold the size for each dimension; each malloc that allocates a
   dimension has the size parameter; we use that parameter to
   initialize the dimension size variable so we can use it later in
   the address calculations.  LEVEL is the dimension we're inspecting.
   Return if STMT is related to an allocation site.  */

static void
analyze_matrix_allocation_site (struct matrix_info *mi, gimple stmt,
				int level, sbitmap visited)
{
  if (gimple_assign_copy_p (stmt) || gimple_assign_cast_p (stmt))
    {
      tree rhs = gimple_assign_rhs1 (stmt);

      if (TREE_CODE (rhs) == SSA_NAME)
	{
	  gimple def = SSA_NAME_DEF_STMT (rhs);

	  analyze_matrix_allocation_site (mi, def, level, visited);
	  return;
	}
      /* If we are back to the original matrix variable then we
         are sure that this is analyzed as an access site.  */
      else if (rhs == mi->decl)
	return;
    }
  /* A result of call to malloc.  */
  else if (is_gimple_call (stmt))
    {
      int call_flags = gimple_call_flags (stmt);

      if (!(call_flags & ECF_MALLOC))
	{
	  mark_min_matrix_escape_level (mi, level, stmt);
	  return;
	}
      else
	{
	  tree malloc_fn_decl;

	  malloc_fn_decl = gimple_call_fndecl (stmt);
	  if (malloc_fn_decl == NULL_TREE)
	    {
	      mark_min_matrix_escape_level (mi, level, stmt);
	      return;
	    }
	  if (DECL_FUNCTION_CODE (malloc_fn_decl) != BUILT_IN_MALLOC)
	    {
	      if (dump_file)
		fprintf (dump_file,
			 "Matrix %s is an argument to function %s\n",
			 get_name (mi->decl), get_name (malloc_fn_decl));
	      mark_min_matrix_escape_level (mi, level, stmt);
	      return;
	    }
	}
      /* This is a call to malloc of level 'level'.
	 mi->max_malloced_level-1 == level  means that we've
	 seen a malloc statement of level 'level' before.
	 If the statement is not the same one that we've
	 seen before, then there's another malloc statement
	 for the same level, which means that we need to mark
	 it escaping.  */
      if (mi->malloc_for_level
	  && mi->max_malloced_level-1 == level
	  && mi->malloc_for_level[level] != stmt)
	{
	  mark_min_matrix_escape_level (mi, level, stmt);
	  return;
	}
      else
	add_allocation_site (mi, stmt, level);
      return;
    }
  /* Looks like we don't know what is happening in this
     statement so be in the safe side and mark it as escaping.  */
  mark_min_matrix_escape_level (mi, level, stmt);
}

/* The transposing decision making.
   In order to to calculate the profitability of transposing, we collect two
   types of information regarding the accesses:
   1. profiling information used to express the hotness of an access, that
   is how often the matrix is accessed by this access site (count of the
   access site).
   2. which dimension in the access site is iterated by the inner
   most loop containing this access.

   The matrix will have a calculated value of weighted hotness for each
   dimension.
   Intuitively the hotness level of a dimension is a function of how
   many times it was the most frequently accessed dimension in the
   highly executed access sites of this matrix.

   As computed by following equation:
   m      n
   __   __
   \    \  dim_hot_level[i] +=
   /_   /_
   j     i
                 acc[j]->dim[i]->iter_by_inner_loop * count(j)

  Where n is the number of dims and m is the number of the matrix
  access sites. acc[j]->dim[i]->iter_by_inner_loop is 1 if acc[j]
  iterates over dim[i] in innermost loop, and is 0 otherwise.

  The organization of the new matrix should be according to the
  hotness of each dimension. The hotness of the dimension implies
  the locality of the elements.*/
static int
analyze_transpose (void **slot, void *data ATTRIBUTE_UNUSED)
{
  struct matrix_info *mi = (struct matrix_info *) *slot;
  int min_escape_l = mi->min_indirect_level_escape;
  struct loop *loop;
  affine_iv iv;
  struct access_site_info *acc_info;
  int i;

  if (min_escape_l < 2 || !mi->access_l)
    {
      if (mi->access_l)
	{
	  FOR_EACH_VEC_ELT (access_site_info_p, mi->access_l, i, acc_info)
	    free (acc_info);
	  VEC_free (access_site_info_p, heap, mi->access_l);

	}
      return 1;
    }
  if (!mi->dim_hot_level)
    mi->dim_hot_level =
      (gcov_type *) xcalloc (min_escape_l, sizeof (gcov_type));


  for (i = 0; VEC_iterate (access_site_info_p, mi->access_l, i, acc_info);
       i++)
    {
      if (gimple_assign_rhs_code (acc_info->stmt) == POINTER_PLUS_EXPR
	  && acc_info->level < min_escape_l)
	{
	  loop = loop_containing_stmt (acc_info->stmt);
	  if (!loop || loop->inner)
	    {
	      free (acc_info);
	      continue;
	    }
	  if (simple_iv (loop, loop, acc_info->offset, &iv, true))
	    {
	      if (iv.step != NULL)
		{
		  HOST_WIDE_INT istep;

		  istep = int_cst_value (iv.step);
		  if (istep != 0)
		    {
		      acc_info->iterated_by_inner_most_loop_p = 1;
		      mi->dim_hot_level[acc_info->level] +=
			gimple_bb (acc_info->stmt)->count;
		    }

		}
	    }
	}
      free (acc_info);
    }
  VEC_free (access_site_info_p, heap, mi->access_l);

  return 1;
}

/* Find the index which defines the OFFSET from base.
   We walk from use to def until we find how the offset was defined.  */
static tree
get_index_from_offset (tree offset, gimple def_stmt)
{
  tree op1, op2, index;

  if (gimple_code (def_stmt) == GIMPLE_PHI)
    return NULL;
  if ((gimple_assign_copy_p (def_stmt) || gimple_assign_cast_p (def_stmt))
      && TREE_CODE (gimple_assign_rhs1 (def_stmt)) == SSA_NAME)
    return get_index_from_offset (offset,
				  SSA_NAME_DEF_STMT (gimple_assign_rhs1 (def_stmt)));
  else if (is_gimple_assign (def_stmt)
	   && gimple_assign_rhs_code (def_stmt) == MULT_EXPR)
    {
      op1 = gimple_assign_rhs1 (def_stmt);
      op2 = gimple_assign_rhs2 (def_stmt);
      if (TREE_CODE (op1) != INTEGER_CST && TREE_CODE (op2) != INTEGER_CST)
	return NULL;
      index = (TREE_CODE (op1) == INTEGER_CST) ? op2 : op1;
      return index;
    }
  else
    return NULL_TREE;
}

/* update MI->dimension_type_size[CURRENT_INDIRECT_LEVEL] with the size
   of the type related to the SSA_VAR, or the type related to the
   lhs of STMT, in the case that it is an MEM_REF.  */
static void
update_type_size (struct matrix_info *mi, gimple stmt, tree ssa_var,
		  int current_indirect_level)
{
  tree lhs;
  HOST_WIDE_INT type_size;

  /* Update type according to the type of the MEM_REF expr.   */
  if (is_gimple_assign (stmt)
      && TREE_CODE (gimple_assign_lhs (stmt)) == MEM_REF)
    {
      lhs = gimple_assign_lhs (stmt);
      gcc_assert (POINTER_TYPE_P
		  (TREE_TYPE (SSA_NAME_VAR (TREE_OPERAND (lhs, 0)))));
      type_size =
	int_size_in_bytes (TREE_TYPE
			   (TREE_TYPE
			    (SSA_NAME_VAR (TREE_OPERAND (lhs, 0)))));
    }
  else
    type_size = int_size_in_bytes (TREE_TYPE (ssa_var));

  /* Record the size of elements accessed (as a whole)
     in the current indirection level (dimension).  If the size of
     elements is not known at compile time, mark it as escaping.  */
  if (type_size <= 0)
    mark_min_matrix_escape_level (mi, current_indirect_level, stmt);
  else
    {
      int l = current_indirect_level;

      if (!mi->dimension_type_size)
	{
	  mi->dimension_type_size
	    = (HOST_WIDE_INT *) xcalloc (l + 1, sizeof (HOST_WIDE_INT));
	  mi->dimension_type_size_len = l + 1;
	}
      else if (mi->dimension_type_size_len < l + 1)
	{
	  mi->dimension_type_size
	    = (HOST_WIDE_INT *) xrealloc (mi->dimension_type_size,
					  (l + 1) * sizeof (HOST_WIDE_INT));
	  memset (&mi->dimension_type_size[mi->dimension_type_size_len],
		  0, (l + 1 - mi->dimension_type_size_len)
		  * sizeof (HOST_WIDE_INT));
	  mi->dimension_type_size_len = l + 1;
	}
      /* Make sure all the accesses in the same level have the same size
         of the type.  */
      if (!mi->dimension_type_size[l])
	mi->dimension_type_size[l] = type_size;
      else if (mi->dimension_type_size[l] != type_size)
	mark_min_matrix_escape_level (mi, l, stmt);
    }
}

/* USE_STMT represents a GIMPLE_CALL, where one of the arguments is the
   ssa var that we want to check because it came from some use of matrix
   MI.  CURRENT_INDIRECT_LEVEL is the indirection level we reached so
   far.  */

static int
analyze_accesses_for_call_stmt (struct matrix_info *mi, tree ssa_var,
				gimple use_stmt, int current_indirect_level)
{
  tree fndecl = gimple_call_fndecl (use_stmt);

  if (gimple_call_lhs (use_stmt))
    {
      tree lhs = gimple_call_lhs (use_stmt);
      struct ssa_acc_in_tree lhs_acc, rhs_acc;

      memset (&lhs_acc, 0, sizeof (lhs_acc));
      memset (&rhs_acc, 0, sizeof (rhs_acc));

      lhs_acc.ssa_var = ssa_var;
      lhs_acc.t_code = ERROR_MARK;
      ssa_accessed_in_tree (lhs, &lhs_acc);
      rhs_acc.ssa_var = ssa_var;
      rhs_acc.t_code = ERROR_MARK;
      ssa_accessed_in_call_rhs (use_stmt, &rhs_acc);

      /* The SSA must be either in the left side or in the right side,
	 to understand what is happening.
	 In case the SSA_NAME is found in both sides we should be escaping
	 at this level because in this case we cannot calculate the
	 address correctly.  */
      if ((lhs_acc.var_found && rhs_acc.var_found
	   && lhs_acc.t_code == MEM_REF)
	  || (!rhs_acc.var_found && !lhs_acc.var_found))
	{
	  mark_min_matrix_escape_level (mi, current_indirect_level, use_stmt);
	  return current_indirect_level;
	}
      gcc_assert (!rhs_acc.var_found || !lhs_acc.var_found);

      /* If we are storing to the matrix at some level, then mark it as
	 escaping at that level.  */
      if (lhs_acc.var_found)
	{
	  int l = current_indirect_level + 1;

	  gcc_assert (lhs_acc.t_code == MEM_REF);
	  mark_min_matrix_escape_level (mi, l, use_stmt);
	  return current_indirect_level;
	}
    }

  if (fndecl)
    {
      if (DECL_FUNCTION_CODE (fndecl) != BUILT_IN_FREE)
	{
	  if (dump_file)
	    fprintf (dump_file,
		     "Matrix %s: Function call %s, level %d escapes.\n",
		     get_name (mi->decl), get_name (fndecl),
		     current_indirect_level);
	  mark_min_matrix_escape_level (mi, current_indirect_level, use_stmt);
	}
      else if (mi->free_stmts[current_indirect_level].stmt != NULL
	       && mi->free_stmts[current_indirect_level].stmt != use_stmt)
	mark_min_matrix_escape_level (mi, current_indirect_level, use_stmt);
      else
	{
	  /*Record the free statements so we can delete them
	     later. */
	  int l = current_indirect_level;

	  mi->free_stmts[l].stmt = use_stmt;
	  mi->free_stmts[l].func = current_function_decl;
	}
    }
  return current_indirect_level;
}

/* USE_STMT represents a phi node of the ssa var that we want to
   check  because it came from some use of matrix
   MI.
   We check all the escaping levels that get to the PHI node
   and make sure they are all the same escaping;
   if not (which is rare) we let the escaping level be the
   minimum level that gets into that PHI because starting from
   that level we cannot expect the behavior of the indirections.
   CURRENT_INDIRECT_LEVEL is the indirection level we reached so far.  */

static void
analyze_accesses_for_phi_node (struct matrix_info *mi, gimple use_stmt,
			       int current_indirect_level, sbitmap visited,
			       bool record_accesses)
{

  struct matrix_access_phi_node tmp_maphi, *maphi, **pmaphi;

  tmp_maphi.phi = use_stmt;
  if ((maphi = (struct matrix_access_phi_node *)
       htab_find (htab_mat_acc_phi_nodes, &tmp_maphi)))
    {
      if (maphi->indirection_level == current_indirect_level)
	return;
      else
	{
	  int level = MIN (maphi->indirection_level,
			   current_indirect_level);
	  size_t j;
	  gimple stmt = NULL;

	  maphi->indirection_level = level;
	  for (j = 0; j < gimple_phi_num_args (use_stmt); j++)
	    {
	      tree def = PHI_ARG_DEF (use_stmt, j);

	      if (gimple_code (SSA_NAME_DEF_STMT (def)) != GIMPLE_PHI)
		stmt = SSA_NAME_DEF_STMT (def);
	    }
	  mark_min_matrix_escape_level (mi, level, stmt);
	}
      return;
    }
  maphi = (struct matrix_access_phi_node *)
    xcalloc (1, sizeof (struct matrix_access_phi_node));
  maphi->phi = use_stmt;
  maphi->indirection_level = current_indirect_level;

  /* Insert to hash table.  */
  pmaphi = (struct matrix_access_phi_node **)
    htab_find_slot (htab_mat_acc_phi_nodes, maphi, INSERT);
  gcc_assert (pmaphi);
  *pmaphi = maphi;

  if (!TEST_BIT (visited, SSA_NAME_VERSION (PHI_RESULT (use_stmt))))
    {
      SET_BIT (visited, SSA_NAME_VERSION (PHI_RESULT (use_stmt)));
      analyze_matrix_accesses (mi, PHI_RESULT (use_stmt),
			       current_indirect_level, false, visited,
			       record_accesses);
      RESET_BIT (visited, SSA_NAME_VERSION (PHI_RESULT (use_stmt)));
    }
}

/* USE_STMT represents an assign statement (the rhs or lhs include
   the ssa var that we want to check  because it came from some use of matrix
   MI.  CURRENT_INDIRECT_LEVEL is the indirection level we reached so far.  */

static int
analyze_accesses_for_assign_stmt (struct matrix_info *mi, tree ssa_var,
				  gimple use_stmt, int current_indirect_level,
				  bool last_op, sbitmap visited,
				  bool record_accesses)
{
  tree lhs = gimple_get_lhs (use_stmt);
  struct ssa_acc_in_tree lhs_acc, rhs_acc;

  memset (&lhs_acc, 0, sizeof (lhs_acc));
  memset (&rhs_acc, 0, sizeof (rhs_acc));

  lhs_acc.ssa_var = ssa_var;
  lhs_acc.t_code = ERROR_MARK;
  ssa_accessed_in_tree (lhs, &lhs_acc);
  rhs_acc.ssa_var = ssa_var;
  rhs_acc.t_code = ERROR_MARK;
  ssa_accessed_in_assign_rhs (use_stmt, &rhs_acc);

  /* The SSA must be either in the left side or in the right side,
     to understand what is happening.
     In case the SSA_NAME is found in both sides we should be escaping
     at this level because in this case we cannot calculate the
     address correctly.  */
  if ((lhs_acc.var_found && rhs_acc.var_found
       && lhs_acc.t_code == MEM_REF)
      || (!rhs_acc.var_found && !lhs_acc.var_found))
    {
      mark_min_matrix_escape_level (mi, current_indirect_level, use_stmt);
      return current_indirect_level;
    }
  gcc_assert (!rhs_acc.var_found || !lhs_acc.var_found);

  /* If we are storing to the matrix at some level, then mark it as
     escaping at that level.  */
  if (lhs_acc.var_found)
    {
      int l = current_indirect_level + 1;

      gcc_assert (lhs_acc.t_code == MEM_REF);

      if (!(gimple_assign_copy_p (use_stmt)
	    || gimple_assign_cast_p (use_stmt))
	  || (TREE_CODE (gimple_assign_rhs1 (use_stmt)) != SSA_NAME))
	mark_min_matrix_escape_level (mi, l, use_stmt);
      else
	{
	  gimple def_stmt = SSA_NAME_DEF_STMT (gimple_assign_rhs1 (use_stmt));
	  analyze_matrix_allocation_site (mi, def_stmt, l, visited);
	  if (record_accesses)
	    record_access_alloc_site_info (mi, use_stmt, NULL_TREE,
					   NULL_TREE, l, true);
	  update_type_size (mi, use_stmt, NULL, l);
	}
      return current_indirect_level;
    }
  /* Now, check the right-hand-side, to see how the SSA variable
     is used.  */
  if (rhs_acc.var_found)
    {
      if (rhs_acc.t_code != MEM_REF
	  && rhs_acc.t_code != POINTER_PLUS_EXPR && rhs_acc.t_code != SSA_NAME)
	{
	  mark_min_matrix_escape_level (mi, current_indirect_level, use_stmt);
	  return current_indirect_level;
	}
      /* If the access in the RHS has an indirection increase the
         indirection level.  */
      if (rhs_acc.t_code == MEM_REF)
	{
	  if (record_accesses)
	    record_access_alloc_site_info (mi, use_stmt, NULL_TREE,
					   NULL_TREE,
					   current_indirect_level, true);
	  current_indirect_level += 1;
	}
      else if (rhs_acc.t_code == POINTER_PLUS_EXPR)
	{
	  gcc_assert (rhs_acc.second_op);
	  if (last_op)
	    /* Currently we support only one PLUS expression on the
	       SSA_NAME that holds the base address of the current
	       indirection level; to support more general case there
	       is a need to hold a stack of expressions and regenerate
	       the calculation later.  */
	    mark_min_matrix_escape_level (mi, current_indirect_level,
					  use_stmt);
	  else
	    {
	      tree index;
	      tree op1, op2;

	      op1 = gimple_assign_rhs1 (use_stmt);
	      op2 = gimple_assign_rhs2 (use_stmt);

	      op2 = (op1 == ssa_var) ? op2 : op1;
	      if (TREE_CODE (op2) == INTEGER_CST)
		index =
		  build_int_cst (TREE_TYPE (op1),
				 TREE_INT_CST_LOW (op2) /
				 int_size_in_bytes (TREE_TYPE (op1)));
	      else
		{
		  index =
		    get_index_from_offset (op2, SSA_NAME_DEF_STMT (op2));
		  if (index == NULL_TREE)
		    {
		      mark_min_matrix_escape_level (mi,
						    current_indirect_level,
						    use_stmt);
		      return current_indirect_level;
		    }
		}
	      if (record_accesses)
		record_access_alloc_site_info (mi, use_stmt, op2,
					       index,
					       current_indirect_level, false);
	    }
	}
      /* If we are storing this level of indirection mark it as
         escaping.  */
      if (lhs_acc.t_code == MEM_REF || TREE_CODE (lhs) != SSA_NAME)
	{
	  int l = current_indirect_level;

	  /* One exception is when we are storing to the matrix
	     variable itself; this is the case of malloc, we must make
	     sure that it's the one and only one call to malloc so
	     we call analyze_matrix_allocation_site to check
	     this out.  */
	  if (TREE_CODE (lhs) != VAR_DECL || lhs != mi->decl)
	    mark_min_matrix_escape_level (mi, current_indirect_level,
					  use_stmt);
	  else
	    {
	      /* Also update the escaping level.  */
	      analyze_matrix_allocation_site (mi, use_stmt, l, visited);
	      if (record_accesses)
		record_access_alloc_site_info (mi, use_stmt, NULL_TREE,
					       NULL_TREE, l, true);
	    }
	}
      else
	{
	  /* We are placing it in an SSA, follow that SSA.  */
	  analyze_matrix_accesses (mi, lhs,
				   current_indirect_level,
				   rhs_acc.t_code == POINTER_PLUS_EXPR,
				   visited, record_accesses);
	}
    }
  return current_indirect_level;
}

/* Given a SSA_VAR (coming from a use statement of the matrix MI),
   follow its uses and level of indirection and find out the minimum
   indirection level it escapes in (the highest dimension) and the maximum
   level it is accessed in (this will be the actual dimension of the
   matrix).  The information is accumulated in MI.
   We look at the immediate uses, if one escapes we finish; if not,
   we make a recursive call for each one of the immediate uses of the
   resulting SSA name.  */
static void
analyze_matrix_accesses (struct matrix_info *mi, tree ssa_var,
			 int current_indirect_level, bool last_op,
			 sbitmap visited, bool record_accesses)
{
  imm_use_iterator imm_iter;
  use_operand_p use_p;

  update_type_size (mi, SSA_NAME_DEF_STMT (ssa_var), ssa_var,
		    current_indirect_level);

  /* We don't go beyond the escaping level when we are performing the
     flattening.  NOTE: we keep the last indirection level that doesn't
     escape.  */
  if (mi->min_indirect_level_escape > -1
      && mi->min_indirect_level_escape <= current_indirect_level)
    return;

/* Now go over the uses of the SSA_NAME and check how it is used in
   each one of them.  We are mainly looking for the pattern MEM_REF,
   then a POINTER_PLUS_EXPR, then MEM_REF etc.  while in between there could
   be any number of copies and casts.  */
  gcc_assert (TREE_CODE (ssa_var) == SSA_NAME);

  FOR_EACH_IMM_USE_FAST (use_p, imm_iter, ssa_var)
  {
    gimple use_stmt = USE_STMT (use_p);
    if (gimple_code (use_stmt) == GIMPLE_PHI)
      /* We check all the escaping levels that get to the PHI node
         and make sure they are all the same escaping;
         if not (which is rare) we let the escaping level be the
         minimum level that gets into that PHI because starting from
         that level we cannot expect the behavior of the indirections.  */

      analyze_accesses_for_phi_node (mi, use_stmt, current_indirect_level,
				     visited, record_accesses);

    else if (is_gimple_call (use_stmt))
      analyze_accesses_for_call_stmt (mi, ssa_var, use_stmt,
				      current_indirect_level);
    else if (is_gimple_assign (use_stmt))
      current_indirect_level =
	analyze_accesses_for_assign_stmt (mi, ssa_var, use_stmt,
					  current_indirect_level, last_op,
					  visited, record_accesses);
  }
}

typedef struct
{
  tree fn;
  gimple stmt;
} check_var_data;

/* A walk_tree function to go over the VAR_DECL, PARM_DECL nodes of
   the malloc size expression and check that those aren't changed
   over the function.  */
static tree
check_var_notmodified_p (tree * tp, int *walk_subtrees, void *data)
{
  basic_block bb;
  tree t = *tp;
  check_var_data *callback_data = (check_var_data*) data;
  tree fn = callback_data->fn;
  gimple_stmt_iterator gsi;
  gimple stmt;

  if (TREE_CODE (t) != VAR_DECL && TREE_CODE (t) != PARM_DECL)
    return NULL_TREE;

  FOR_EACH_BB_FN (bb, DECL_STRUCT_FUNCTION (fn))
  {
    for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
      {
	stmt = gsi_stmt (gsi);
	if (!is_gimple_assign (stmt) && !is_gimple_call (stmt))
	  continue;
	if (gimple_get_lhs (stmt) == t)
	  {
	    callback_data->stmt = stmt;
	    return t;
	  }
      }
  }
  *walk_subtrees = 1;
  return NULL_TREE;
}

/* Go backwards in the use-def chains and find out the expression
   represented by the possible SSA name in STMT, until it is composed
   of only VAR_DECL, PARM_DECL and INT_CST.  In case of phi nodes
   we make sure that all the arguments represent the same subexpression,
   otherwise we fail.  */

static tree
can_calculate_stmt_before_stmt (gimple stmt, sbitmap visited)
{
  tree op1, op2, res;
  enum tree_code code;

  switch (gimple_code (stmt))
    {
    case GIMPLE_ASSIGN:
      code = gimple_assign_rhs_code (stmt);
      op1 = gimple_assign_rhs1 (stmt);

      switch (code)
	{
	case POINTER_PLUS_EXPR:
	case PLUS_EXPR:
	case MINUS_EXPR:
	case MULT_EXPR:

	  op2 = gimple_assign_rhs2 (stmt);
	  op1 = can_calculate_expr_before_stmt (op1, visited);
	  if (!op1)
	    return NULL_TREE;
	  op2 = can_calculate_expr_before_stmt (op2, visited);
	  if (op2)
	    return fold_build2 (code, gimple_expr_type (stmt), op1, op2);
	  return NULL_TREE;

	CASE_CONVERT:
	  res = can_calculate_expr_before_stmt (op1, visited);
	  if (res != NULL_TREE)
	    return build1 (code, gimple_expr_type (stmt), res);
	  else
	    return NULL_TREE;

	default:
	  if (gimple_assign_single_p (stmt))
	    return can_calculate_expr_before_stmt (op1, visited);
	  else
	    return NULL_TREE;
	}

    case GIMPLE_PHI:
      {
	size_t j;

	res = NULL_TREE;
	/* Make sure all the arguments represent the same value.  */
	for (j = 0; j < gimple_phi_num_args (stmt); j++)
	  {
	    tree new_res;
	    tree def = PHI_ARG_DEF (stmt, j);

	    new_res = can_calculate_expr_before_stmt (def, visited);
	    if (res == NULL_TREE)
	      res = new_res;
	    else if (!new_res || !expressions_equal_p (res, new_res))
	      return NULL_TREE;
	  }
	return res;
      }

    default:
      return NULL_TREE;
    }
}

/* Go backwards in the use-def chains and find out the expression
   represented by the possible SSA name in EXPR, until it is composed
   of only VAR_DECL, PARM_DECL and INT_CST.  In case of phi nodes
   we make sure that all the arguments represent the same subexpression,
   otherwise we fail.  */
static tree
can_calculate_expr_before_stmt (tree expr, sbitmap visited)
{
  gimple def_stmt;
  tree res;

  switch (TREE_CODE (expr))
    {
    case SSA_NAME:
      /* Case of loop, we don't know to represent this expression.  */
      if (TEST_BIT (visited, SSA_NAME_VERSION (expr)))
	return NULL_TREE;

      SET_BIT (visited, SSA_NAME_VERSION (expr));
      def_stmt = SSA_NAME_DEF_STMT (expr);
      res = can_calculate_stmt_before_stmt (def_stmt, visited);
      RESET_BIT (visited, SSA_NAME_VERSION (expr));
      return res;
    case VAR_DECL:
    case PARM_DECL:
    case INTEGER_CST:
      return expr;

    default:
      return NULL_TREE;
    }
}

/* There should be only one allocation function for the dimensions
   that don't escape. Here we check the allocation sites in this
   function. We must make sure that all the dimensions are allocated
   using malloc and that the malloc size parameter expression could be
   pre-calculated before the call to the malloc of dimension 0.

   Given a candidate matrix for flattening -- MI -- check if it's
   appropriate for flattening -- we analyze the allocation
   sites that we recorded in the previous analysis.  The result of the
   analysis is a level of indirection (matrix dimension) in which the
   flattening is safe.  We check the following conditions:
   1. There is only one allocation site for each dimension.
   2. The allocation sites of all the dimensions are in the same
      function.
      (The above two are being taken care of during the analysis when
      we check the allocation site).
   3. All the dimensions that we flatten are allocated at once; thus
      the total size must be known before the allocation of the
      dimension 0 (top level) -- we must make sure we represent the
      size of the allocation as an expression of global parameters or
      constants and that those doesn't change over the function.  */

static int
check_allocation_function (void **slot, void *data ATTRIBUTE_UNUSED)
{
  int level;
  struct matrix_info *mi = (struct matrix_info *) *slot;
  sbitmap visited;

  if (!mi->malloc_for_level)
    return 1;

  visited = sbitmap_alloc (num_ssa_names);

  /* Do nothing if the current function is not the allocation
     function of MI.  */
  if (mi->allocation_function_decl != current_function_decl
      /* We aren't in the main allocation function yet.  */
      || !mi->malloc_for_level[0])
    return 1;

  for (level = 1; level < mi->max_malloced_level; level++)
    if (!mi->malloc_for_level[level])
      break;

  mark_min_matrix_escape_level (mi, level, NULL);

  /* Check if the expression of the size passed to malloc could be
     pre-calculated before the malloc of level 0.  */
  for (level = 1; level < mi->min_indirect_level_escape; level++)
    {
      gimple call_stmt;
      tree size;
      struct malloc_call_data mcd = {NULL, NULL_TREE, NULL_TREE};

      call_stmt = mi->malloc_for_level[level];

      /* Find the correct malloc information.  */
      collect_data_for_malloc_call (call_stmt, &mcd);

      /* No need to check anticipation for constants.  */
      if (TREE_CODE (mcd.size_var) == INTEGER_CST)
	{
	  if (!mi->dimension_size)
	    {
	      mi->dimension_size =
		(tree *) xcalloc (mi->min_indirect_level_escape,
				  sizeof (tree));
	      mi->dimension_size_orig =
		(tree *) xcalloc (mi->min_indirect_level_escape,
				  sizeof (tree));
	    }
	  mi->dimension_size[level] = mcd.size_var;
	  mi->dimension_size_orig[level] = mcd.size_var;
	  continue;
	}
      /* ??? Here we should also add the way to calculate the size
         expression not only know that it is anticipated.  */
      sbitmap_zero (visited);
      size = can_calculate_expr_before_stmt (mcd.size_var, visited);
      if (size == NULL_TREE)
	{
	  mark_min_matrix_escape_level (mi, level, call_stmt);
	  if (dump_file)
	    fprintf (dump_file,
		     "Matrix %s: Cannot calculate the size of allocation, escaping at level %d\n",
		     get_name (mi->decl), level);
	  break;
	}
      if (!mi->dimension_size)
	{
	  mi->dimension_size =
	    (tree *) xcalloc (mi->min_indirect_level_escape, sizeof (tree));
	  mi->dimension_size_orig =
	    (tree *) xcalloc (mi->min_indirect_level_escape, sizeof (tree));
	}
      mi->dimension_size[level] = size;
      mi->dimension_size_orig[level] = size;
    }

  /* We don't need those anymore.  */
  for (level = mi->min_indirect_level_escape;
       level < mi->max_malloced_level; level++)
    mi->malloc_for_level[level] = NULL;
  return 1;
}

/* Track all access and allocation sites.  */
static void
find_sites_in_func (bool record)
{
  sbitmap visited_stmts_1;

  gimple_stmt_iterator gsi;
  gimple stmt;
  basic_block bb;
  struct matrix_info tmpmi, *mi;

  visited_stmts_1 = sbitmap_alloc (num_ssa_names);

  FOR_EACH_BB (bb)
  {
    for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
      {
	tree lhs;

	stmt = gsi_stmt (gsi);
	lhs = gimple_get_lhs (stmt);
	if (lhs != NULL_TREE
	    && TREE_CODE (lhs) == VAR_DECL)
	  {
	    tmpmi.decl = lhs;
	    if ((mi = (struct matrix_info *) htab_find (matrices_to_reorg,
							&tmpmi)))
	      {
		sbitmap_zero (visited_stmts_1);
		analyze_matrix_allocation_site (mi, stmt, 0, visited_stmts_1);
	      }
	  }
	if (is_gimple_assign (stmt)
	    && gimple_assign_single_p (stmt)
	    && TREE_CODE (lhs) == SSA_NAME
	    && TREE_CODE (gimple_assign_rhs1 (stmt)) == VAR_DECL)
	  {
	    tmpmi.decl = gimple_assign_rhs1 (stmt);
	    if ((mi = (struct matrix_info *) htab_find (matrices_to_reorg,
							&tmpmi)))
	      {
		sbitmap_zero (visited_stmts_1);
		analyze_matrix_accesses (mi, lhs, 0,
					 false, visited_stmts_1, record);
	      }
	  }
      }
  }
  sbitmap_free (visited_stmts_1);
}

/* Traverse the use-def chains to see if there are matrices that
   are passed through pointers and we cannot know how they are accessed.
   For each SSA-name defined by a global variable of our interest,
   we traverse the use-def chains of the SSA and follow the indirections,
   and record in what level of indirection the use of the variable
   escapes.  A use of a pointer escapes when it is passed to a function,
   stored into memory or assigned (except in malloc and free calls).  */

static void
record_all_accesses_in_func (void)
{
  unsigned i;
  sbitmap visited_stmts_1;

  visited_stmts_1 = sbitmap_alloc (num_ssa_names);

  for (i = 0; i < num_ssa_names; i++)
    {
      struct matrix_info tmpmi, *mi;
      tree ssa_var = ssa_name (i);
      tree rhs, lhs;

      if (!ssa_var
	  || !is_gimple_assign (SSA_NAME_DEF_STMT (ssa_var))
	  || !gimple_assign_single_p (SSA_NAME_DEF_STMT (ssa_var)))
	continue;
      rhs = gimple_assign_rhs1 (SSA_NAME_DEF_STMT (ssa_var));
      lhs = gimple_assign_lhs (SSA_NAME_DEF_STMT (ssa_var));
      if (TREE_CODE (rhs) != VAR_DECL && TREE_CODE (lhs) != VAR_DECL)
	continue;

      /* If the RHS is a matrix that we want to analyze, follow the def-use
         chain for this SSA_VAR and check for escapes or apply the
         flattening.  */
      tmpmi.decl = rhs;
      if ((mi = (struct matrix_info *) htab_find (matrices_to_reorg, &tmpmi)))
	{
	  /* This variable will track the visited PHI nodes, so we can limit
	     its size to the maximum number of SSA names.  */
	  sbitmap_zero (visited_stmts_1);
	  analyze_matrix_accesses (mi, ssa_var,
				   0, false, visited_stmts_1, true);

	}
    }
  sbitmap_free (visited_stmts_1);
}

/* Used when we want to convert the expression: RESULT = something *
   ORIG to RESULT = something * NEW_VAL. If ORIG and NEW_VAL are power
   of 2, shift operations can be done, else division and
   multiplication.  */

static tree
compute_offset (HOST_WIDE_INT orig, HOST_WIDE_INT new_val, tree result)
{

  int x, y;
  tree result1, ratio, log, orig_tree, new_tree;

  x = exact_log2 (orig);
  y = exact_log2 (new_val);

  if (x != -1 && y != -1)
    {
      if (x == y)
        return result;
      else if (x > y)
        {
          log = build_int_cst (TREE_TYPE (result), x - y);
          result1 =
            fold_build2 (LSHIFT_EXPR, TREE_TYPE (result), result, log);
          return result1;
        }
      log = build_int_cst (TREE_TYPE (result), y - x);
      result1 = fold_build2 (RSHIFT_EXPR, TREE_TYPE (result), result, log);

      return result1;
    }
  orig_tree = build_int_cst (TREE_TYPE (result), orig);
  new_tree = build_int_cst (TREE_TYPE (result), new_val);
  ratio = fold_build2 (TRUNC_DIV_EXPR, TREE_TYPE (result), result, orig_tree);
  result1 = fold_build2 (MULT_EXPR, TREE_TYPE (result), ratio, new_tree);

  return result1;
}


/* We know that we are allowed to perform matrix flattening (according to the
   escape analysis), so we traverse the use-def chains of the SSA vars
   defined by the global variables pointing to the matrices of our interest.
   in each use of the SSA we calculate the offset from the base address
   according to the following equation:

     a[I1][I2]...[Ik] , where D1..Dk is the length of each dimension and the
     escaping level is m <= k, and a' is the new allocated matrix,
     will be translated to :

       b[I(m+1)]...[Ik]

       where
       b = a' + I1*D2...*Dm + I2*D3...Dm + ... + Im
                                                      */

static int
transform_access_sites (void **slot, void *data ATTRIBUTE_UNUSED)
{
  gimple_stmt_iterator gsi;
  struct matrix_info *mi = (struct matrix_info *) *slot;
  int min_escape_l = mi->min_indirect_level_escape;
  struct access_site_info *acc_info;
  enum tree_code code;
  int i;

  if (min_escape_l < 2 || !mi->access_l)
    return 1;
  for (i = 0; VEC_iterate (access_site_info_p, mi->access_l, i, acc_info);
       i++)
    {
      /* This is possible because we collect the access sites before
         we determine the final minimum indirection level.  */
      if (acc_info->level >= min_escape_l)
	{
	  free (acc_info);
	  continue;
	}
      if (acc_info->is_alloc)
	{
	  if (acc_info->level >= 0 && gimple_bb (acc_info->stmt))
	    {
	      ssa_op_iter iter;
	      tree def;
	      gimple stmt = acc_info->stmt;
	      tree lhs;

	      FOR_EACH_SSA_TREE_OPERAND (def, stmt, iter, SSA_OP_DEF)
		mark_sym_for_renaming (SSA_NAME_VAR (def));
	      gsi = gsi_for_stmt (stmt);
	      gcc_assert (is_gimple_assign (acc_info->stmt));
	      lhs = gimple_assign_lhs (acc_info->stmt);
	      if (TREE_CODE (lhs) == SSA_NAME
		  && acc_info->level < min_escape_l - 1)
		{
		  imm_use_iterator imm_iter;
		  use_operand_p use_p;
		  gimple use_stmt;

		  FOR_EACH_IMM_USE_STMT (use_stmt, imm_iter, lhs)
		    FOR_EACH_IMM_USE_ON_STMT (use_p, imm_iter)
		  {
		    tree rhs, tmp;
		    gimple new_stmt;

		    gcc_assert (gimple_assign_rhs_code (acc_info->stmt)
				== MEM_REF);
		    /* Emit convert statement to convert to type of use.  */
		    tmp = create_tmp_var (TREE_TYPE (lhs), "new");
		    add_referenced_var (tmp);
		    rhs = gimple_assign_rhs1 (acc_info->stmt);
		    rhs = fold_convert (TREE_TYPE (tmp),
					TREE_OPERAND (rhs, 0));
		    new_stmt = gimple_build_assign (tmp, rhs);
		    tmp = make_ssa_name (tmp, new_stmt);
		    gimple_assign_set_lhs (new_stmt, tmp);
		    gsi = gsi_for_stmt (acc_info->stmt);
		    gsi_insert_after (&gsi, new_stmt, GSI_SAME_STMT);
		    SET_USE (use_p, tmp);
		  }
		}
	      if (acc_info->level < min_escape_l - 1)
		gsi_remove (&gsi, true);
	    }
	  free (acc_info);
	  continue;
	}
      code = gimple_assign_rhs_code (acc_info->stmt);
      if (code == MEM_REF
	  && acc_info->level < min_escape_l - 1)
	{
	  /* Replace the MEM_REF with NOP (cast) usually we are casting
	     from "pointer to type" to "type".  */
	  tree t =
	    build1 (NOP_EXPR, TREE_TYPE (gimple_assign_rhs1 (acc_info->stmt)),
		    TREE_OPERAND (gimple_assign_rhs1 (acc_info->stmt), 0));
	  gimple_assign_set_rhs_code (acc_info->stmt, NOP_EXPR);
	  gimple_assign_set_rhs1 (acc_info->stmt, t);
	}
      else if (code == POINTER_PLUS_EXPR
	       && acc_info->level < (min_escape_l))
	{
	  imm_use_iterator imm_iter;
	  use_operand_p use_p;

	  tree offset;
	  int k = acc_info->level;
	  tree num_elements, total_elements;
	  tree tmp1;
	  tree d_size = mi->dimension_size[k];

	  /* We already make sure in the analysis that the first operand
	     is the base and the second is the offset.  */
	  offset = acc_info->offset;
	  if (mi->dim_map[k] == min_escape_l - 1)
	    {
	      if (!check_transpose_p || mi->is_transposed_p == false)
		tmp1 = offset;
	      else
		{
		  tree new_offset;

		  new_offset =
		    compute_offset (mi->dimension_type_size[min_escape_l],
				    mi->dimension_type_size[k + 1], offset);

		  total_elements = new_offset;
		  if (new_offset != offset)
		    {
		      gsi = gsi_for_stmt (acc_info->stmt);
		      tmp1 = force_gimple_operand_gsi (&gsi, total_elements,
						       true, NULL,
						       true, GSI_SAME_STMT);
		    }
		  else
		    tmp1 = offset;
		}
	    }
	  else
	    {
	      d_size = mi->dimension_size[mi->dim_map[k] + 1];
	      num_elements =
		fold_build2 (MULT_EXPR, sizetype, fold_convert (sizetype, acc_info->index),
			    fold_convert (sizetype, d_size));
	      add_referenced_var (d_size);
	      gsi = gsi_for_stmt (acc_info->stmt);
	      tmp1 = force_gimple_operand_gsi (&gsi, num_elements, true,
					       NULL, true, GSI_SAME_STMT);
	    }
	  /* Replace the offset if needed.  */
	  if (tmp1 != offset)
	    {
	      if (TREE_CODE (offset) == SSA_NAME)
		{
		  gimple use_stmt;

		  FOR_EACH_IMM_USE_STMT (use_stmt, imm_iter, offset)
		    FOR_EACH_IMM_USE_ON_STMT (use_p, imm_iter)
		      if (use_stmt == acc_info->stmt)
		        SET_USE (use_p, tmp1);
		}
	      else
		{
		  gcc_assert (TREE_CODE (offset) == INTEGER_CST);
		  gimple_assign_set_rhs2 (acc_info->stmt, tmp1);
		  update_stmt (acc_info->stmt);
		}
	    }
	}
      /* ??? meanwhile this happens because we record the same access
         site more than once; we should be using a hash table to
         avoid this and insert the STMT of the access site only
         once.
         else
         gcc_unreachable (); */
      free (acc_info);
    }
  VEC_free (access_site_info_p, heap, mi->access_l);

  update_ssa (TODO_update_ssa);
#ifdef ENABLE_CHECKING
  verify_ssa (true);
#endif
  return 1;
}

/* Sort A array of counts. Arrange DIM_MAP to reflect the new order.  */

static void
sort_dim_hot_level (gcov_type * a, int *dim_map, int n)
{
  int i, j, tmp1;
  gcov_type tmp;

  for (i = 0; i < n - 1; i++)
    {
      for (j = 0; j < n - 1 - i; j++)
	{
	  if (a[j + 1] < a[j])
	    {
	      tmp = a[j];	/* swap a[j] and a[j+1]      */
	      a[j] = a[j + 1];
	      a[j + 1] = tmp;
	      tmp1 = dim_map[j];
	      dim_map[j] = dim_map[j + 1];
	      dim_map[j + 1] = tmp1;
	    }
	}
    }
}

/* Replace multiple mallocs (one for each dimension) to one malloc
   with the size of DIM1*DIM2*...*DIMN*size_of_element
   Make sure that we hold the size in the malloc site inside a
   new global variable; this way we ensure that the size doesn't
   change and it is accessible from all the other functions that
   uses the matrix.  Also, the original calls to free are deleted,
   and replaced by a new call to free the flattened matrix.  */

static int
transform_allocation_sites (void **slot, void *data ATTRIBUTE_UNUSED)
{
  int i;
  struct matrix_info *mi;
  tree type, oldfn, prev_dim_size;
  gimple call_stmt_0, use_stmt;
  struct cgraph_node *c_node;
  struct cgraph_edge *e;
  gimple_stmt_iterator gsi;
  struct malloc_call_data mcd = {NULL, NULL_TREE, NULL_TREE};
  HOST_WIDE_INT element_size;

  imm_use_iterator imm_iter;
  use_operand_p use_p;
  tree old_size_0, tmp;
  int min_escape_l;
  int id;

  mi = (struct matrix_info *) *slot;

  min_escape_l = mi->min_indirect_level_escape;

  if (!mi->malloc_for_level)
    mi->min_indirect_level_escape = 0;

  if (mi->min_indirect_level_escape < 2)
    return 1;

  mi->dim_map = (int *) xcalloc (mi->min_indirect_level_escape, sizeof (int));
  for (i = 0; i < mi->min_indirect_level_escape; i++)
    mi->dim_map[i] = i;
  if (check_transpose_p)
    {
      int i;

      if (dump_file)
	{
	  fprintf (dump_file, "Matrix %s:\n", get_name (mi->decl));
	  for (i = 0; i < min_escape_l; i++)
	    {
	      fprintf (dump_file, "dim %d before sort ", i);
	      if (mi->dim_hot_level)
		fprintf (dump_file,
			 "count is  " HOST_WIDEST_INT_PRINT_DEC "  \n",
			 mi->dim_hot_level[i]);
	    }
	}
      sort_dim_hot_level (mi->dim_hot_level, mi->dim_map,
			  mi->min_indirect_level_escape);
      if (dump_file)
	for (i = 0; i < min_escape_l; i++)
	  {
	    fprintf (dump_file, "dim %d after sort\n", i);
	    if (mi->dim_hot_level)
	      fprintf (dump_file, "count is  " HOST_WIDE_INT_PRINT_DEC
		       "  \n", (HOST_WIDE_INT) mi->dim_hot_level[i]);
	  }
      for (i = 0; i < mi->min_indirect_level_escape; i++)
	{
	  if (dump_file)
	    fprintf (dump_file, "dim_map[%d] after sort %d\n", i,
		     mi->dim_map[i]);
	  if (mi->dim_map[i] != i)
	    {
	      if (dump_file)
		fprintf (dump_file,
			 "Transposed dimensions: dim %d is now dim %d\n",
			 mi->dim_map[i], i);
	      mi->is_transposed_p = true;
	    }
	}
    }
  else
    {
      for (i = 0; i < mi->min_indirect_level_escape; i++)
	mi->dim_map[i] = i;
    }
  /* Call statement of allocation site of level 0.  */
  call_stmt_0 = mi->malloc_for_level[0];

  /* Finds the correct malloc information.  */
  collect_data_for_malloc_call (call_stmt_0, &mcd);

  mi->dimension_size[0] = mcd.size_var;
  mi->dimension_size_orig[0] = mcd.size_var;
  /* Make sure that the variables in the size expression for
     all the dimensions (above level 0) aren't modified in
     the allocation function.  */
  for (i = 1; i < mi->min_indirect_level_escape; i++)
    {
      tree t;
      check_var_data data;

      /* mi->dimension_size must contain the expression of the size calculated
         in check_allocation_function.  */
      gcc_assert (mi->dimension_size[i]);

      data.fn = mi->allocation_function_decl;
      data.stmt = NULL;
      t = walk_tree_without_duplicates (&(mi->dimension_size[i]),
					check_var_notmodified_p,
					&data);
      if (t != NULL_TREE)
	{
	  mark_min_matrix_escape_level (mi, i, data.stmt);
	  break;
	}
    }

  if (mi->min_indirect_level_escape < 2)
    return 1;

  /* Since we should make sure that the size expression is available
     before the call to malloc of level 0.  */
  gsi = gsi_for_stmt (call_stmt_0);

  /* Find out the size of each dimension by looking at the malloc
     sites and create a global variable to hold it.
     We add the assignment to the global before the malloc of level 0.  */

  /* To be able to produce gimple temporaries.  */
  oldfn = current_function_decl;
  current_function_decl = mi->allocation_function_decl;
  push_cfun (DECL_STRUCT_FUNCTION (mi->allocation_function_decl));

  /* Set the dimension sizes as follows:
     DIM_SIZE[i] = DIM_SIZE[n] * ... * DIM_SIZE[i]
     where n is the maximum non escaping level.  */
  element_size = mi->dimension_type_size[mi->min_indirect_level_escape];
  prev_dim_size = NULL_TREE;

  for (i = mi->min_indirect_level_escape - 1; i >= 0; i--)
    {
      tree dim_size, dim_var;
      gimple stmt;
      tree d_type_size;

      /* Now put the size expression in a global variable and initialize it to
         the size expression before the malloc of level 0.  */
      dim_var =
	add_new_static_var (TREE_TYPE
			    (mi->dimension_size_orig[mi->dim_map[i]]));
      type = TREE_TYPE (mi->dimension_size_orig[mi->dim_map[i]]);

      /* DIM_SIZE = MALLOC_SIZE_PARAM / TYPE_SIZE.  */
      /* Find which dim ID becomes dim I.  */
      for (id = 0; id < mi->min_indirect_level_escape; id++)
	if (mi->dim_map[id] == i)
	  break;
       d_type_size =
        build_int_cst (type, mi->dimension_type_size[id + 1]);
      if (!prev_dim_size)
	prev_dim_size = build_int_cst (type, element_size);
      if (!check_transpose_p && i == mi->min_indirect_level_escape - 1)
	{
	  dim_size = mi->dimension_size_orig[id];
	}
      else
	{
	  dim_size =
	    fold_build2 (TRUNC_DIV_EXPR, type, mi->dimension_size_orig[id],
			 d_type_size);

	  dim_size = fold_build2 (MULT_EXPR, type, dim_size, prev_dim_size);
	}
      dim_size = force_gimple_operand_gsi (&gsi, dim_size, true, NULL,
					   true, GSI_SAME_STMT);
      /* GLOBAL_HOLDING_THE_SIZE = DIM_SIZE.  */
      stmt = gimple_build_assign (dim_var, dim_size);
      mark_symbols_for_renaming (stmt);
      gsi_insert_before (&gsi, stmt, GSI_SAME_STMT);

      prev_dim_size = mi->dimension_size[i] = dim_var;
    }
  update_ssa (TODO_update_ssa);
  /* Replace the malloc size argument in the malloc of level 0 to be
     the size of all the dimensions.  */
  c_node = cgraph_node (mi->allocation_function_decl);
  old_size_0 = gimple_call_arg (call_stmt_0, 0);
  tmp = force_gimple_operand_gsi (&gsi, mi->dimension_size[0], true,
				  NULL, true, GSI_SAME_STMT);
  if (TREE_CODE (old_size_0) == SSA_NAME)
    {
      FOR_EACH_IMM_USE_STMT (use_stmt, imm_iter, old_size_0)
	FOR_EACH_IMM_USE_ON_STMT (use_p, imm_iter)
	if (use_stmt == call_stmt_0)
	SET_USE (use_p, tmp);
    }
  /* When deleting the calls to malloc we need also to remove the edge from
     the call graph to keep it consistent.  Notice that cgraph_edge may
     create a new node in the call graph if there is no node for the given
     declaration; this shouldn't be the case but currently there is no way to
     check this outside of "cgraph.c".  */
  for (i = 1; i < mi->min_indirect_level_escape; i++)
    {
      gimple_stmt_iterator gsi;

      gimple call_stmt = mi->malloc_for_level[i];
      gcc_assert (is_gimple_call (call_stmt));
      e = cgraph_edge (c_node, call_stmt);
      gcc_assert (e);
      cgraph_remove_edge (e);
      gsi = gsi_for_stmt (call_stmt);
      /* Remove the call stmt.  */
      gsi_remove (&gsi, true);
      /* Remove the assignment of the allocated area.  */
      FOR_EACH_IMM_USE_STMT (use_stmt, imm_iter,
			     gimple_call_lhs (call_stmt))
      {
	gsi = gsi_for_stmt (use_stmt);
	gsi_remove (&gsi, true);
      }
    }
  update_ssa (TODO_update_ssa);
#ifdef ENABLE_CHECKING
  verify_ssa (true);
#endif
  /* Delete the calls to free.  */
  for (i = 1; i < mi->min_indirect_level_escape; i++)
    {
      gimple_stmt_iterator gsi;

      /* ??? wonder why this case is possible but we failed on it once.  */
      if (!mi->free_stmts[i].stmt)
	continue;

      c_node = cgraph_node (mi->free_stmts[i].func);
      gcc_assert (is_gimple_call (mi->free_stmts[i].stmt));
      e = cgraph_edge (c_node, mi->free_stmts[i].stmt);
      gcc_assert (e);
      cgraph_remove_edge (e);
      current_function_decl = mi->free_stmts[i].func;
      set_cfun (DECL_STRUCT_FUNCTION (mi->free_stmts[i].func));
      gsi = gsi_for_stmt (mi->free_stmts[i].stmt);
      gsi_remove (&gsi, true);
    }
  /* Return to the previous situation.  */
  current_function_decl = oldfn;
  pop_cfun ();
  return 1;

}


/* Print out the results of the escape analysis.  */
static int
dump_matrix_reorg_analysis (void **slot, void *data ATTRIBUTE_UNUSED)
{
  struct matrix_info *mi = (struct matrix_info *) *slot;

  if (!dump_file)
    return 1;
  fprintf (dump_file, "Matrix \"%s\"; Escaping Level: %d, Num Dims: %d,",
	   get_name (mi->decl), mi->min_indirect_level_escape, mi->num_dims);
  fprintf (dump_file, " Malloc Dims: %d, ", mi->max_malloced_level);
  fprintf (dump_file, "\n");
  if (mi->min_indirect_level_escape >= 2)
    fprintf (dump_file, "Flattened %d dimensions \n",
	     mi->min_indirect_level_escape);
  return 1;
}

/* Perform matrix flattening.  */

static unsigned int
matrix_reorg (void)
{
  struct cgraph_node *node;

  if (profile_info)
    check_transpose_p = true;
  else
    check_transpose_p = false;
  /* If there are hand written vectors, we skip this optimization.  */
  for (node = cgraph_nodes; node; node = node->next)
    if (!may_flatten_matrices (node))
      return 0;
  matrices_to_reorg = htab_create (37, mtt_info_hash, mtt_info_eq, mat_free);
  /* Find and record all potential matrices in the program.  */
  find_matrices_decl ();
  /* Analyze the accesses of the matrices (escaping analysis).  */
  for (node = cgraph_nodes; node; node = node->next)
    if (node->analyzed)
      {
	tree temp_fn;

	temp_fn = current_function_decl;
	current_function_decl = node->decl;
	push_cfun (DECL_STRUCT_FUNCTION (node->decl));
	bitmap_obstack_initialize (NULL);
	gimple_register_cfg_hooks ();

	if (!gimple_in_ssa_p (cfun))
	  {
	    free_dominance_info (CDI_DOMINATORS);
	    free_dominance_info (CDI_POST_DOMINATORS);
	    pop_cfun ();
	    current_function_decl = temp_fn;
	    bitmap_obstack_release (NULL);

	    return 0;
	  }

#ifdef ENABLE_CHECKING
	verify_flow_info ();
#endif

	if (!matrices_to_reorg)
	  {
	    free_dominance_info (CDI_DOMINATORS);
	    free_dominance_info (CDI_POST_DOMINATORS);
	    pop_cfun ();
	    current_function_decl = temp_fn;
	    bitmap_obstack_release (NULL);

	    return 0;
	  }

	/* Create htap for phi nodes.  */
	htab_mat_acc_phi_nodes = htab_create (37, mat_acc_phi_hash,
					      mat_acc_phi_eq, free);
	if (!check_transpose_p)
	  find_sites_in_func (false);
	else
	  {
	    find_sites_in_func (true);
	    loop_optimizer_init (LOOPS_NORMAL);
	    if (current_loops)
	      scev_initialize ();
	    htab_traverse (matrices_to_reorg, analyze_transpose, NULL);
	    if (current_loops)
	      {
		scev_finalize ();
		loop_optimizer_finalize ();
		current_loops = NULL;
	      }
	  }
	/* If the current function is the allocation function for any of
	   the matrices we check its allocation and the escaping level.  */
	htab_traverse (matrices_to_reorg, check_allocation_function, NULL);
	free_dominance_info (CDI_DOMINATORS);
	free_dominance_info (CDI_POST_DOMINATORS);
	pop_cfun ();
	current_function_decl = temp_fn;
	bitmap_obstack_release (NULL);
      }
  htab_traverse (matrices_to_reorg, transform_allocation_sites, NULL);
  /* Now transform the accesses.  */
  for (node = cgraph_nodes; node; node = node->next)
    if (node->analyzed)
      {
	/* Remember that allocation sites have been handled.  */
	tree temp_fn;

	temp_fn = current_function_decl;
	current_function_decl = node->decl;
	push_cfun (DECL_STRUCT_FUNCTION (node->decl));
	bitmap_obstack_initialize (NULL);
	gimple_register_cfg_hooks ();
	record_all_accesses_in_func ();
	htab_traverse (matrices_to_reorg, transform_access_sites, NULL);
        cgraph_rebuild_references ();
	free_dominance_info (CDI_DOMINATORS);
	free_dominance_info (CDI_POST_DOMINATORS);
	pop_cfun ();
	current_function_decl = temp_fn;
	bitmap_obstack_release (NULL);
      }
  htab_traverse (matrices_to_reorg, dump_matrix_reorg_analysis, NULL);

  current_function_decl = NULL;
  set_cfun (NULL);
  matrices_to_reorg = NULL;
  return 0;
}


/* The condition for matrix flattening to be performed.  */
static bool
gate_matrix_reorg (void)
{
  return flag_ipa_matrix_reorg && flag_whole_program;
}

struct simple_ipa_opt_pass pass_ipa_matrix_reorg =
{
 {
  SIMPLE_IPA_PASS,
  "matrix-reorg",		/* name */
  gate_matrix_reorg,		/* gate */
  matrix_reorg,			/* execute */
  NULL,				/* sub */
  NULL,				/* next */
  0,				/* static_pass_number */
  TV_NONE,			/* tv_id */
  0,				/* properties_required */
  0,				/* properties_provided */
  0,				/* properties_destroyed */
  0,				/* todo_flags_start */
  TODO_dump_cgraph | TODO_dump_func	/* todo_flags_finish */
 }
};
