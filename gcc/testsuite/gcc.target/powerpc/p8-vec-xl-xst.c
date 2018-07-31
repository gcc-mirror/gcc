/* { dg-do compile { target { le } } } */
/* { dg-skip-if "" { powerpc*-*-darwin* } } */
/* { dg-require-effective-target powerpc_p8vector_ok } */
/* { dg-skip-if "do not override -mcpu" { powerpc*-*-* } { "-mcpu=*" } { "-mcpu=power8" } } */
/* { dg-options "-mcpu=power8 -O2 -mno-fold-gimple" } */
/* { dg-prune-output "gimple folding of rs6000 builtins has been disabled." } */

/* Verify fix for problem where vec_xl and vec_xst are not recognized
   for the vector char and vector short cases on P8 only.  */

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

/* { dg-final { scan-assembler-times "lxvd2x"   4 } } */
/* { dg-final { scan-assembler-times "stxvd2x"  4 } } */
/* { dg-final { scan-assembler-times "xxpermdi" 8 } } */
