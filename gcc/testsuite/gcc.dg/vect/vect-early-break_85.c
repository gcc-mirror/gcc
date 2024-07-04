/* { dg-add-options vect_early_break } */
/* { dg-require-effective-target vect_early_break_hw } */
/* { dg-require-effective-target vect_int } */

/* { dg-final { scan-tree-dump "LOOP VECTORIZED" "vect" } } */

#include "tree-vect.h"

#ifndef N
#define N 5
#endif
int vect_a[N] = { 5, 4, 8, 4, 6 };
unsigned vect_b[N] = { 0 };

__attribute__ ((noinline, noipa))
unsigned test4(int x)
{
 unsigned ret = 0;
 for (int i = 0; i < N; i++)
 {
   if (vect_a[i] > x)
     return 1;
   vect_a[i] = x;
   
 }
 return ret;
}

extern void abort ();

int main ()
{
  check_vect ();

  if (test4 (7) != 1)
    abort ();

  if (vect_b[2] != 0 && vect_b[1] == 0)
    abort ();
}
