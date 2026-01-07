/* { dg-add-options vect_early_break } */
/* { dg-require-effective-target vect_sizes_16B_8B } */
/* { dg-require-effective-target vect_early_break_hw } */
/* { dg-require-effective-target vect_int } */

/* { dg-final { scan-tree-dump "LOOP VECTORIZED" "vect" } } */

#include "tree-vect.h"

__attribute__ ((noipa))
int f (int a[12], int b[12], int n)
{
#ifdef __arm__
    a = __builtin_assume_aligned (a, 8);
    b = __builtin_assume_aligned (b, 8);
#else
    a = __builtin_assume_aligned (a, 16);
    b = __builtin_assume_aligned (b, 16);
#endif
    for (int i = 0; i < n; i++)
      {
        if (b[i] == 0)
          return 0;
        if (a[0] > b[i])
          return 1;
      }
    return 2;
}

int main ()
{
   check_vect ();

   int *a = 0;
   int b[12] = {0};
   return f (a, b, 10);
}

/* { dg-final { scan-tree-dump "not hoisting invariant load due to early break" "vect" } } */
