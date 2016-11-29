/* Verify that overloaded built-ins for vec_add with int
   inputs produce the right results.  */

/* { dg-do compile } */
/* { dg-require-effective-target powerpc_altivec_ok } */
/* { dg-options "-maltivec" } */

#include <altivec.h>

vector signed int
test1 (vector bool int x, vector signed int y)
{
  return vec_add (x, y);
}

vector signed int
test2 (vector signed int x, vector bool int y)
{
  return vec_add (x, y);
}

vector signed int
test3 (vector signed int x, vector signed int y)
{
  return vec_add (x, y);
}

vector unsigned int
test4 (vector bool int x, vector unsigned int y)
{
  return vec_add (x, y);
}

vector unsigned int
test5 (vector unsigned int x, vector bool int y)
{
  return vec_add (x, y);
}

vector unsigned int
test6 (vector unsigned int x, vector unsigned int y)
{
  return vec_add (x, y);
}

/* { dg-final { scan-assembler-times "vadduwm" 6 } } */
