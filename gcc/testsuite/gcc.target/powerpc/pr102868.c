/* { dg-do compile } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-options "-O2 -mvsx" } */

#include <altivec.h>
vector float b = {0.0f, 0.0f, 0.0f, 0.0f};


vector float foo1 (vector float x)
{
  vector int c = {0, 1, 4, 5};
  return __builtin_shuffle (x, b, c);
}

vector float foo2 (vector float x)
{
  vector int c = {2, 3, 4, 5};
  return __builtin_shuffle (x, b, c);
}

vector float foo3 (vector float x)
{
  vector int c = {0, 1, 6, 7};
  return __builtin_shuffle (x, b, c);
}

vector float foo4 (vector float x)
{
  vector int c = {2, 3, 6, 7};
  return __builtin_shuffle (x, b, c);
}

vector unsigned char foo5 (vector unsigned char x)
{
  vector unsigned char c = {0, 1, 2, 3, 4, 5, 6, 7, 0, 1, 2, 3, 4, 5, 6, 7};
  return __builtin_shuffle (x, c);
}

vector unsigned char foo6 (vector unsigned char x)
{
  vector unsigned char c = {8, 9, 10, 11, 12, 13, 14, 15, 8, 9, 10, 11, 12, 13, 14, 15};
  return __builtin_shuffle (x, c);
}

vector unsigned char foo7 (vector unsigned char x)
{
  vector unsigned char c = {8, 9, 10, 11, 12, 13, 14, 15, 0, 1, 2, 3, 4, 5, 6, 7};
  return __builtin_shuffle (x, c);
}

/* { dg-final { scan-assembler-times {\mxxpermdi\M} 7 { target has_arch_pwr9 } } } */
/* { dg-final { scan-assembler-times {\mxxpermdi\M} 7 { target { {! has_arch_pwr9} && be } } } } */
/* { dg-final { scan-assembler-times {\mxxpermdi\M} 11 { target { {! has_arch_pwr9} && le } } } } */
