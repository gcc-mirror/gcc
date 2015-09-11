/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */


int f(int *r)
{
  r[1] = 0;
  r[2] = 1;
  return r[1];
}


/* { dg-final { scan-tree-dump-times "return 0;" 1 "optimized" } } */

