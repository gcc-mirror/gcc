/* { dg-do compile } */
/* { dg-require-effective-target powerpc_altivec_ok } */
/* { dg-options "-maltivec -mno-vsx -mno-power8-vector -O0" } */

#include <altivec.h>

/* Test Altivec built-ins added for version 1.1 of ELFv2 ABI.  */

vector signed int vsia, vsib;

void foo (vector signed int *vsir)
{
  *vsir++ = vec_addc (vsia, vsib);
}

/* { dg-final { scan-assembler-times "vaddcuw" 1 } } */
