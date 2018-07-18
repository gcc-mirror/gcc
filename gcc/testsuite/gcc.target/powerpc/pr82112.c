/* PR target/82112 */
/* { dg-do compile } */
/* { dg-require-effective-target powerpc_altivec_ok } */
/* { dg-options "-maltivec -std=gnu90" } */

#include <altivec.h>

struct __attribute__((aligned (16))) S { unsigned char c[64]; } bar (void);
vector unsigned char v;

void
foo (void)
{
  vec_ld (0, bar ().c);	/* { dg-error "invalid parameter combination for AltiVec intrinsic" } */
  vec_st (v, 0, bar ().c);	/* { dg-error "invalid parameter combination for AltiVec intrinsic" } */
}
