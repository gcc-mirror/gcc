/* { dg-do run } */
/* { dg-options "-O2" } */
/* { dg-require-effective-target vmx_hw } */

/* Test vector merge for 16-bit element size,
   it will abort if the RTL pattern isn't expected.  */

#include "altivec.h"

__attribute__((noipa))
signed short elem_2 (vector signed short a, vector signed short b)
{
  vector signed short c = vec_mergeh (a,b);
  return vec_extract (c, 2);
}

__attribute__((noipa))
unsigned short elem_7 (vector unsigned short a, vector unsigned short b)
{
  vector unsigned short c = vec_mergel (a,b);
  return vec_extract (c, 7);
}

int
main ()
{
  vector unsigned short v1 = {3, 22, 12, 34, 5, 25, 30, 11};
  vector unsigned short v2 = {84, 168, 267, 966, 65, 399, 999, 99};
  signed short x1 = elem_2 ((vector signed short) v1, (vector signed short) v2);
  unsigned short x2 = elem_7 (v1, v2);

  if (x1 != 22 || x2 != 99)
    __builtin_abort ();

  return 0;
}

