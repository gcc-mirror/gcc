/* { dg-add-options vect_early_break } */
/* { dg-require-effective-target vect_early_break_hw } */
/* { dg-require-effective-target vect_int } */

/* { dg-final { scan-tree-dump "LOOP VECTORIZED" "vect" { target { ! "x86_64-*-* i?86-*-*" } } } } */

#include <stdbool.h>

#include "tree-vect.h"

#ifndef N
#define N 17
#endif
bool vect_a[N] = { false, false, true, false, false, false,
                   false, false, false, false, false, false,
                   false, false, false, false, false };
unsigned vect_b[N] = { 0 };

__attribute__ ((noinline, noipa))
unsigned test4(bool x)
{
 unsigned ret = 0;
 for (int i = 0; i < N; i++)
 {
   if (vect_a[i] == x)
     return 1;
   vect_a[i] = x;
   
 }
 return ret;
}

extern void abort ();

int main ()
{
  check_vect ();

  if (test4 (true) != 1)
    abort ();

  if (vect_b[2] != 0 && vect_b[1] == 0)
    abort ();
}
