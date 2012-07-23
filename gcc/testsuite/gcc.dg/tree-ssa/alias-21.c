/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */


int f(int *r)
{
  r[0] = 0;
  r[1] = 1;
  return r[0];
}


/* { dg-final { scan-tree-dump-times "return 0;" 1 "optimized" } } */
/* { dg-final { cleanup-tree-dump "optimized" } } */

