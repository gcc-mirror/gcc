/* { dg-do compile } */ 
/* { dg-require-effective-target powerpc_p9vector_ok } */
/* { dg-options "-mdejagnu-cpu=power9 -O2 " } */ 

#include <altivec.h>

unsigned int foo (vector unsigned char a, vector unsigned char b) {
  return vec_first_match_or_eos_index (a, b);
}
/* { dg-final { scan-assembler-not {\mrldicl\M} } } */
