/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-ccp1" } */

int
test(int* a, int* b)
{
  __INTPTR_TYPE__ delta = (int*)__builtin_assume_aligned(b, 32)
               - (int*)__builtin_assume_aligned(a, 32);
  __INTPTR_TYPE__ x = delta % 8;
  return (x == 0);
}

/* { dg-final { scan-tree-dump "return 1;" "ccp1" } } */
