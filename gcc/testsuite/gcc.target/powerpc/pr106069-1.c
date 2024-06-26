/* { dg-do run } */
/* { dg-options "-O2" } */
/* { dg-require-effective-target vmx_hw } */

/* Test vector merge for 8-bit element size,
   it will abort if the RTL pattern isn't expected.  */

#include "altivec.h"

__attribute__((noipa))
signed char elem_6 (vector signed char a, vector signed char b)
{
  vector signed char c = vec_mergeh (a,b);
  return vec_extract (c, 6);
}

__attribute__((noipa))
unsigned char elem_15 (vector unsigned char a, vector unsigned char b)
{
  vector unsigned char c = vec_mergel (a,b);
  return vec_extract (c, 15);
}

int
main ()
{
  vector unsigned char v1
    = {3, 33, 22, 12, 34, 14, 5, 25, 30, 11, 0, 21, 17, 27, 38, 8};
  vector unsigned char v2
    = {81, 82, 83, 84, 68, 67, 66, 65, 99, 100, 101, 102, 250, 125, 0, 6};
  signed char x1 = elem_6 ((vector signed char) v1, (vector signed char) v2);
  unsigned char x2 = elem_15 (v1, v2);

  if (x1 != 12 || x2 != 6)
    __builtin_abort ();

  return 0;
}

