/* { dg-do compile } */ 
/* { dg-options "-O2 -fdump-tree-vrp1-details" } */

/* We should simplify one ASSERT_EXPR from a relational
   into an equality test.  */
/* { dg-final { scan-tree-dump-times "Folded into:\[^\r\n\]*ASSERT_EXPR\*\[^\r\n\]* == 1" 1 "vrp1" } } */

/* And simplification of the ASSERT_EXPR leads to a jump threading opportunity.  */
/* { dg-final { scan-tree-dump-times "Threaded" 1 "vrp1" } } */

extern void abort (void) __attribute__ ((__nothrow__, __leaf__))
  __attribute__ ((__noreturn__));

union gimple_statement_d;
typedef union gimple_statement_d *gimple;



union gimple_statement_d
{
  unsigned num_ops;
};

void
gimple_assign_set_rhs_with_ops_1 (int code, gimple stmt, unsigned new_rhs_ops)
{

  stmt->num_ops = new_rhs_ops + 1;
  if (stmt->num_ops <= 1)
    abort ();
  if (new_rhs_ops > 1)
    if (stmt->num_ops <= 2)
      abort ();
  if (new_rhs_ops > 2)
      abort ();
}
