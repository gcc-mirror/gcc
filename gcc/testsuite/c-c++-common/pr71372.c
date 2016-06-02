/* PR c++/71372 */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

void
foo (volatile int *p, int q)
{
  *(volatile int *)p = 0;
  *(p + (q - q) + 1) = 0;
  *(p + (q - q) + 2) = 0;
  *(p + (q - q) + 3) = 0;
}

/* { dg-final { scan-tree-dump-times " ={v} " 4 "optimized" } } */
