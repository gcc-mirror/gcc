/* { dg-options "-O2 -fdump-tree-optimized-raw" } */

typedef __PTRDIFF_TYPE__ ptrdiff_t;
typedef __SIZE_TYPE__ size_t;

void
cmp1 (int *ptr1, int *ptr2)
{
  unsigned char x1 = ptr2 - ptr1;
  x1 += 0x40;
  ptrdiff_t x2 = (ptrdiff_t) x1 * (ptrdiff_t) 4;

  ptrdiff_t y = ((char *) ptr2 - (char *) ptr1) + (ptrdiff_t) 0x100;

  size_t z = (char *) ptr2 - (char *) ptr1;
  z += (size_t) 0x100;

  if (x2 != y && x2 != (ptrdiff_t) z)
    __builtin_abort ();
}

/* { dg-final { scan-tree-dump {gimple_call <} "optimized" } } */
