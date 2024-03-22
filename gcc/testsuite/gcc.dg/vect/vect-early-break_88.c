/* { dg-add-options vect_early_break } */
/* { dg-require-effective-target vect_early_break_hw } */
/* { dg-require-effective-target vect_int } */

/* { dg-additional-options "-Ofast --param vect-partial-vector-usage=2" } */
/* { dg-final { scan-tree-dump "LOOP VECTORIZED" "vect" { target { ! "arm*-*-*" } } } } */

#include "tree-vect.h"

#ifndef N
#define N 5
#endif
float vect_a[N] = { 5.1f, 4.2f, 8.0f, 4.25f, 6.5f };
unsigned vect_b[N] = { 0 };

__attribute__ ((noinline, noipa))
unsigned test4(double x)
{
 unsigned ret = 0;
 for (int i = 0; i < N; i++)
 {
   if (vect_a[i] > x)
     break;
   vect_a[i] = x;
   
 }
 return ret;
}

extern void abort ();

int main ()
{
  check_vect ();

  if (test4 (7.0) != 0)
    abort ();

  if (vect_b[2] != 0 && vect_b[1] == 0)
    abort ();
}
