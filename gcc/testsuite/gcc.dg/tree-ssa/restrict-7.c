/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-fre1" } */

int
f (int *__restrict__ *__restrict__ *__restrict__ a, int *b)
{
  *b = 1;
  ***a  = 2;
  return *b;
}

/* { dg-final { scan-tree-dump-times "return 1" 1 "fre1" } } */
