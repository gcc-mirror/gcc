/* { dg-do compile { target { le } } } */
/* { dg-skip-if "" { powerpc*-*-darwin* } } */
/* { dg-require-effective-target powerpc_p8vector_ok } */
/* { dg-skip-if "do not override -mcpu" { powerpc*-*-* } { "-mcpu=*" } { "-mcpu=power8" } } */
/* { dg-options "-mcpu=power8 -O2" } */

/* Verify fix for problem where vec_xl and vec_xst are not recognized
   for the vector char and vector short cases on P8 only.
   This test duplicates p8-vec-xl-xst.c , except that it allows gimple-folding,
   which changes the expected codegen.  */

#include <altivec.h>

vector unsigned char
foo (unsigned char * address)
{
  return __builtin_vec_xl (0, address);
}

void
bar (vector unsigned char x, unsigned char * address)
{
  __builtin_vec_xst (x, 0, address);
}

vector unsigned short
foot (unsigned short * address)
{
  return __builtin_vec_xl (0, address);
}

void
bart (vector unsigned short x, unsigned short * address)
{
  __builtin_vec_xst (x, 0, address);
}

vector unsigned char
fool (unsigned char * address)
{
  return vec_xl (0, address);
}

void
barl (vector unsigned char x, unsigned char * address)
{
  vec_xst (x, 0, address);
}

vector unsigned short
footle (unsigned short * address)
{
  return vec_xl (0, address);
}

void
bartle (vector unsigned short x, unsigned short * address)
{
  vec_xst (x, 0, address);
}

/* { dg-final { scan-assembler-times "lvx" 4 } } */
/* { dg-final { scan-assembler-times "stvx"  4 } } */
/* { dg-final { scan-assembler-times "xxpermdi" 0 } } */
