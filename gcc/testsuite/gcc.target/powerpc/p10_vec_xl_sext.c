/* { dg-do compile } */
/* { dg-require-effective-target int128 } */
/* { dg-require-effective-target power10_ok } */
/* { dg-options "-mdejagnu-cpu=power10 -O2" } */

#include <altivec.h>

vector signed __int128
foo1 (signed long a, signed char *b)
{
  return vec_xl_sext (a, b);
}

vector signed __int128
foo2 (signed long a, signed short *b)
{
  return vec_xl_sext (a, b);
}

vector signed __int128
foo3 (signed long a, signed int *b)
{
  return vec_xl_sext (a, b);
}

vector signed __int128
foo4 (signed long a, signed long long *b)
{
  return vec_xl_sext (a, b);
}

/* { dg-final { scan-assembler-times {\mvextsd2q\M} 4 } } */
/* { dg-final { scan-assembler-times {\mvextsb2d\M} 1 } } */
/* { dg-final { scan-assembler-times {\mvextsh2d\M} 1 } } */
/* { dg-final { scan-assembler-times {\mvextsw2d\M} 1 } } */
