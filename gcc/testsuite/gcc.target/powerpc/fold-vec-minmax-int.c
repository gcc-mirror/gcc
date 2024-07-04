/* Verify that overloaded built-ins for vec_min with int
   inputs produce the right results.  */

/* { dg-do compile } */
/* { dg-options "-maltivec -O2" } */
/* { dg-require-effective-target powerpc_altivec } */

#include <altivec.h>

vector signed int
test3_min (vector signed int x, vector signed int y)
{
  return vec_min (x, y);
}

vector unsigned int
test6_min (vector unsigned int x, vector unsigned int y)
{
  return vec_min (x, y);
}

vector signed int
test3_max (vector signed int x, vector signed int y)
{
  return vec_max (x, y);
}

vector unsigned int
test6_max (vector unsigned int x, vector unsigned int y)
{
  return vec_max (x, y);
}

vector signed int
test4_min (vector bool int x, vector signed int y)
{
  return vec_min (x, y);
}

vector signed int
test5_min (vector signed int x, vector bool int y)
{
  return vec_min (x, y);
}

vector unsigned int
test7_min (vector bool int x, vector unsigned int y)
{
  return vec_min (x, y);
}

vector unsigned int
test8_min (vector unsigned int x, vector bool int y)
{
  return vec_min (x, y);
}

/* { dg-final { scan-assembler-times "vminsw" 3 } } */
/* { dg-final { scan-assembler-times "vmaxsw" 1 } } */
/* { dg-final { scan-assembler-times "vminuw" 3 } } */
/* { dg-final { scan-assembler-times "vmaxuw" 1 } } */
