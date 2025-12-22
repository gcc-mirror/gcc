/* { dg-add-options vect_early_break } */
/* { dg-require-effective-target vect_early_break_hw } */
/* { dg-require-effective-target vect_int } */

/* { dg-final { scan-tree-dump "LOOP VECTORIZED" "vect" } } */

#include "tree-vect.h"

__attribute__ ((noipa))
int f (int a[12], int b[12], int n)
{
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

/* { dg-final { scan-tree-dump-times "not hoisting invariant load due to early break" 0 "vect" { xfail *-*-* } } } */
