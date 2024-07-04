/* Verify that overloaded built-ins for vec_min with char
   inputs produce the right results.  */

/* { dg-do compile } */
/* { dg-options "-maltivec" } */
/* { dg-require-effective-target powerpc_altivec } */

#include <altivec.h>

vector signed char
test3_min (vector signed char x, vector signed char y)
{
  return vec_min (x, y);
}

vector unsigned char
test6_min (vector unsigned char x, vector unsigned char y)
{
  return vec_min (x, y);
}

vector signed char
test1_min (vector bool char x, vector signed char y)
{
  return vec_min (x, y);
}

vector unsigned char
test2_min (vector bool char x, vector unsigned char y)
{
  return vec_min (x, y);
}

vector unsigned char
test4_min (vector unsigned char x, vector bool char y)
{
  return vec_min (x, y);
}

vector signed char
test1_max (vector signed char x, vector signed char y)
{
  return vec_max (x, y);
}

vector unsigned char
test2_max (vector unsigned char x, vector unsigned char y)
{
  return vec_max (x, y);
}

/* { dg-final { scan-assembler-times "vminsb" 2 } } */
/* { dg-final { scan-assembler-times "vmaxsb" 1 } } */
/* { dg-final { scan-assembler-times "vminub" 3 } } */
/* { dg-final { scan-assembler-times "vmaxub" 1 } } */
