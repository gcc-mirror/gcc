/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-dom1" } */

typedef long unsigned int size_t;
extern void abort (void) __attribute__ ((__noreturn__));
union tree_node;
typedef union tree_node *tree;
union gimple_statement_d;
typedef union gimple_statement_d *gimple;
typedef const union gimple_statement_d *const_gimple;

enum gimple_code
{
  GIMPLE_RETURN = 10,
};





struct gimple_statement_base
{


  enum gimple_code code:8;
};


enum gimple_statement_structure_enum
{
  xyz
};






union gimple_statement_d
{
  struct gimple_statement_base gsbase;
};





extern size_t const gimple_ops_offset_[];


extern enum gimple_statement_structure_enum const gss_for_code_[];


static inline enum gimple_code
gimple_code (const_gimple g)
{
  return g->gsbase.code;
}




static inline enum gimple_statement_structure_enum
gss_for_code (enum gimple_code code)
{
  return gss_for_code_[code];
}




static inline enum gimple_statement_structure_enum
gimple_statement_structure (gimple gs)
{
  return gss_for_code (gimple_code (gs));
}


static inline tree *
gimple_ops (gimple gs)
{
  size_t off;
  off = gimple_ops_offset_[gimple_statement_structure (gs)];
  return (tree *) ((char *) gs + off);
}


static inline void
gimple_set_op (gimple gs, unsigned i, tree op)
{
  gimple_ops (gs)[i] = op;
}

void
gimple_return_set_retval (gimple gs, tree retval)
{
  const_gimple __gs = (gs);
  if (gimple_code (__gs) != (GIMPLE_RETURN))
    abort ();
  gimple_set_op (gs, 0, retval);
}
/* { dg-final { scan-tree-dump-times "gss_for_code_.10." 1 "dom1"} } */
/* { dg-final { cleanup-tree-dump "dom1" } } */

