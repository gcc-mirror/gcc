/* Verify that overloaded built-ins for vec_min with short
   inputs produce the right results.  */

/* { dg-do compile } */
/* { dg-options "-maltivec" } */
/* { dg-require-effective-target powerpc_altivec } */

#include <altivec.h>

vector signed short
test3_min (vector signed short x, vector signed short y)
{
  return vec_min (x, y);
}

vector unsigned short
test6_min (vector unsigned short x, vector unsigned short y)
{
  return vec_min (x, y);
}

vector signed short
test3_max (vector signed short x, vector signed short y)
{
  return vec_max (x, y);
}

vector unsigned short
test6_max (vector unsigned short x, vector unsigned short y)
{
  return vec_max (x, y);
}

vector unsigned short
test4_min (vector bool short x, vector unsigned short y)
{
  return vec_min (x, y);
}

vector unsigned short
test5_min (vector unsigned short x, vector bool short y)
{
  return vec_min (x, y);
}

vector signed short
test7_min (vector bool short x, vector signed short y)
{
  return vec_min (x, y);
}

vector signed short
test8_min (vector signed short x, vector bool short y)
{
  return vec_min (x, y);
}

/* { dg-final { scan-assembler-times "vminsh" 3 } } */
/* { dg-final { scan-assembler-times "vmaxsh" 1 } } */
/* { dg-final { scan-assembler-times "vminuh" 3 } } */
/* { dg-final { scan-assembler-times "vmaxuh" 1 } } */
