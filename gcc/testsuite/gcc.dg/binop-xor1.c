/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

int
foo (int a, int b, int c)
{
  return ((a && !b && c) || (!a && b && c));
}

/* { dg-final { scan-tree-dump-times "\\\^" 1 "optimized" { xfail logical_op_short_circuit } } } */
