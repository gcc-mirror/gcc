/* { dg-do run } */
/* { dg-require-effective-target vmx_hw } */
/* { dg-options "-O2 -mdejagnu-cpu=power8 -maltivec" } */

/* The expansion for vector character multiply introduces a vperm operation.
   This tests that the swap optimization to remove swaps by changing the
   vperm mask results in correct code.  */

#include <altivec.h>

void abort ();

vector unsigned char r;
vector unsigned char v =
  { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15 };
vector unsigned char i =
  { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16 };
vector unsigned char e =
  {0, 2, 6, 12, 20, 30, 42, 56, 72, 90, 110, 132, 156, 182, 210, 240};

int main ()
{
  int j;
  r = v * i;
  if (!vec_all_eq (r, e))
    abort ();
  return 0;
}
