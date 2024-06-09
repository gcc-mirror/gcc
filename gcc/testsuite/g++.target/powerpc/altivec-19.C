/* { dg-do compile } */ 
/* { dg-options "-mdejagnu-cpu=power9 -mvsx -O2 " } */ 
/* { dg-require-effective-target powerpc_vsx } */

#include <altivec.h>

unsigned int foo (vector unsigned char a, vector unsigned char b) {
  return vec_first_match_or_eos_index (a, b);
}
/* { dg-final { scan-assembler-not {\mrldicl\M} } } */
