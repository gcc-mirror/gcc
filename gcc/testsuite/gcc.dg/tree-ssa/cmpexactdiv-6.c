/* { dg-options "-O2 -fdump-tree-optimized-raw" } */

typedef __INTPTR_TYPE__ intptr_t;

int
f1 (int x, int y)
{
  if ((x & 1) || (y & 1))
    __builtin_unreachable ();
  x /= 2;
  y /= 2;
  return x < y;
}

int
f2 (void *ptr1, void *ptr2, void *ptr3)
{
  ptr1 = __builtin_assume_aligned (ptr1, 4);
  ptr2 = __builtin_assume_aligned (ptr2, 4);
  ptr3 = __builtin_assume_aligned (ptr3, 4);
  intptr_t diff1 = (intptr_t) ptr1 - (intptr_t) ptr2;
  intptr_t diff2 = (intptr_t) ptr1 - (intptr_t) ptr3;
  diff1 /= 2;
  diff2 /= 2;
  return diff1 < diff2;
}

/* { dg-final { scan-tree-dump-not {<[a-z]*_div_expr,} "optimized" } } */
/* { dg-final { scan-tree-dump-not {<rshift_expr,} "optimized" } } */
