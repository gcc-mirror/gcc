/* { dg-do compile } */ 
/* { dg-options "-O2 -fdump-tree-dom2-details -w" } */
typedef long unsigned int size_t;
union tree_node;
typedef union tree_node *tree;
typedef union gimple_statement_d *gimple;
typedef const union gimple_statement_d *const_gimple;
union gimple_statement_d
{
  unsigned num_ops;
  tree exp;
};

unsigned int x;
static inline tree
gimple_op (const_gimple gs, unsigned i)
{
  if (!(i < gs->num_ops))
    abort ();
  return gs->exp;
}

unsigned char
scan_function (gimple stmt)
{
  unsigned i;
  for (i = 0; i < stmt->num_ops - 3 ; i++)
    gimple_call_arg (stmt, i);
  gimple_op (stmt, 1);
}

/* The test which bypasses the loop is simplified prior to DOM to check
   that stmt->num_ops - 3 != 0.  When that test is false, we can derive
   a value for stmt->num_ops.  That in turn allows us to thread the jump
   for the conditional at the start of the call to gimple_op.  */
/* { dg-final { scan-tree-dump-times "Threaded" 1 "dom2"} } */
