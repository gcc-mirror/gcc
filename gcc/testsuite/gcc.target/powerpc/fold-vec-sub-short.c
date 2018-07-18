/* Verify that overloaded built-ins for vec_sub with short
   inputs produce the right results.  */

/* { dg-do compile } */
/* { dg-require-effective-target powerpc_altivec_ok } */
/* { dg-options "-maltivec" } */

#include <altivec.h>

vector signed short
test1 (vector bool short x, vector signed short y)
{
  return vec_sub (x, y);
}

vector signed short
test2 (vector signed short x, vector bool short y)
{
  return vec_sub (x, y);
}

vector signed short
test3 (vector signed short x, vector signed short y)
{
  return vec_sub (x, y);
}

vector unsigned short
test4 (vector bool short x, vector unsigned short y)
{
  return vec_sub (x, y);
}

vector unsigned short
test5 (vector unsigned short x, vector bool short y)
{
  return vec_sub (x, y);
}

vector unsigned short
test6 (vector unsigned short x, vector unsigned short y)
{
  return vec_sub (x, y);
}

/* { dg-final { scan-assembler-times "vsubuhm" 6 } } */

