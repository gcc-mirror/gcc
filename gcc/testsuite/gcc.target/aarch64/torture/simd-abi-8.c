/* { dg-do compile } */
/* { dg-options "-std=gnu99 -mlittle-endian" } */
/* { dg-skip-if "" { *-*-* } { "-O0" } { "" } } */

#include <arm_neon.h>

void __attribute__ ((aarch64_vector_pcs)) f (void);

void
g (int64x2x4_t *ptr)
{
  register int64x2x4_t copy asm ("v8") = *ptr;
  int64x2x4_t save;
  asm volatile ("" : "=w" (save) : "0" (copy));
  f ();
  *ptr = save;
}

/* { dg-final { scan-assembler-times {\tld1\t} 1 } } */
/* { dg-final { scan-assembler-times {\tst1\t} 1 } } */
/* { dg-final { scan-assembler-not {\tld[pr]\tq} } } */
/* { dg-final { scan-assembler-not {\tst[pr]\tq} } } */
