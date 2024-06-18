/* Verify that overloaded built-ins for vec_add with char
   inputs produce the right results.  */

/* { dg-do compile } */
/* { dg-options "-maltivec" } */
/* { dg-require-effective-target powerpc_altivec } */

#include <altivec.h>

vector signed char
test1 (vector bool char x, vector signed char y)
{
  return vec_add (x, y);
}

vector signed char
test2 (vector signed char x, vector bool char y)
{
  return vec_add (x, y);
}

vector signed char
test3 (vector signed char x, vector signed char y)
{
  return vec_add (x, y);
}

vector unsigned char
test4 (vector bool char x, vector unsigned char y)
{
  return vec_add (x, y);
}

vector unsigned char
test5 (vector unsigned char x, vector bool char y)
{
  return vec_add (x, y);
}

vector unsigned char
test6 (vector unsigned char x, vector unsigned char y)
{
  return vec_add (x, y);
}

/* { dg-final { scan-assembler-times "vaddubm" 6 } } */
