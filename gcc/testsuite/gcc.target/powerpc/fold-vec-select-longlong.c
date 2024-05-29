/* Verify that overloaded built-ins for vec_sel with long long
   inputs produce the right code.  */

/* { dg-do compile } */
/* { dg-options "-mvsx -O2" } */
/* { dg-require-effective-target powerpc_vsx } */

#include <altivec.h>

vector bool long long
test1_0 (vector bool long long x,vector bool long long y, vector bool long long z)
{
  return vec_sel (x, y, z);
}

vector bool long long
test1_1 (vector bool long long x, vector bool long long y, vector unsigned long long z)
{
  return vec_sel (x, y, z);
}

vector signed long long
test3_0 (vector signed long long x, vector signed long long y, vector bool long long z)
{
  return vec_sel (x, y, z);
}

vector unsigned long long
test3_1 (vector unsigned long long x, vector unsigned long long y, vector bool long long z)
{
  return vec_sel (x, y, z);
}

/* { dg-final { scan-assembler-times "xxsel" 4 } } */
