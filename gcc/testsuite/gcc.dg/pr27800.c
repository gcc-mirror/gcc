/* { dg-do compile } */
/* { dg-options "-fdump-tree-gimple" } */

int iii (int a, int b, int c)
{
  return a ? b : c;
}

/* Verify we end up with two assignments and not an extra copy
   resulting from another temporary generated from gimplify_cond_expr.  */
/* { dg-final { scan-tree-dump-times " = " 2 "gimple" } } */
