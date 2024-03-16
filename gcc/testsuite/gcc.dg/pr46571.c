/* { dg-do compile } */
/* { dg-options "-O2 -fpermissive -fcompare-debug -w" } */

typedef union tree_node *tree;
typedef unsigned int source_location;
enum tree_code
{
  MINUS_EXPR,
  MULT_EXPR,
};
struct tree_omp_clause
{
  union omp_clause_subcode
  {
    enum tree_code reduction_code;
  } subcode;
};
union tree_node
{
  struct tree_omp_clause omp_clause;
};
enum tree_index
{
  TI_ERROR_MARK,
};
typedef struct
{
  unsigned allocatable:1;
  unsigned dimension:1;
  unsigned codimension:1;
  unsigned external:1;
  unsigned optional:1;
  unsigned pointer:1;
  unsigned contiguous:1;
  unsigned referenced:1;
} symbol_attribute;
typedef unsigned int gfc_char_t;
typedef struct gfc_linebuf
{
  source_location location;
} gfc_linebuf;
typedef struct
{
  gfc_char_t *nextc;
  gfc_linebuf *lb;
} locus;
typedef struct
{
  struct gfc_symbol *sym;
  struct gfc_namelist *next;
} gfc_namelist;
enum
{
  OMP_LIST_PLUS,
  OMP_LIST_REDUCTION_FIRST = OMP_LIST_PLUS,
  OMP_LIST_MULT,
  OMP_LIST_SUB,
  OMP_LIST_NUM
};
typedef struct gfc_omp_clauses
{
  gfc_namelist *lists[OMP_LIST_NUM];
} gfc_omp_clauses;
typedef struct gfc_symbol
{
  symbol_attribute attr;
} gfc_symbol;
typedef struct gfc_code
{
  locus loc;
  union
  {
    gfc_omp_clauses *omp_clauses;
  } ext;
} gfc_code;
typedef struct
{
} stmtblock_t;

static tree
gfc_trans_omp_reduction_list (gfc_namelist * namelist, tree list,
			      enum tree_code reduction_code, locus where)
{
  for (; namelist != ((void *) 0); namelist = namelist->next)
    if (namelist->sym->attr.referenced)
      {
	tree node = build_omp_clause (where.lb->location);
	node->omp_clause.subcode.reduction_code = reduction_code;
	gfc_trans_omp_array_reduction (namelist->sym, where);
      }
}

static tree
gfc_trans_omp_clauses (stmtblock_t * block, gfc_omp_clauses * clauses,
		       locus where)
{
  tree omp_clauses = (tree) ((void *) 0);
  int list;
  for (list = 0; list < OMP_LIST_NUM; list++)
    {
      gfc_namelist *n = clauses->lists[list];
      enum tree_code reduction_code;
      if (n == ((void *) 0))
	continue;
      switch (list)
	{
	case OMP_LIST_MULT:
	  reduction_code = MULT_EXPR;
	  break;
	case OMP_LIST_SUB:
	  reduction_code = MINUS_EXPR;
	}
      gfc_trans_omp_reduction_list (n, omp_clauses, reduction_code, where);
    }
}

void
gfc_trans_omp_parallel_workshare (gfc_code * code)
{
  stmtblock_t block;
  gfc_trans_omp_clauses (&block, code->ext.omp_clauses, code->loc);
}
