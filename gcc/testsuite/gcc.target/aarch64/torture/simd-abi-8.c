/* { dg-do compile } */
/* { dg-options "-std=gnu99" } */
/* { dg-skip-if "" { *-*-* } { "-O0" } { "" } } */
/* { dg-require-effective-target aarch64_little_endian } */

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

/* { dg-final { scan-assembler {\tld[pr]\tq} } } */
/* { dg-final { scan-assembler {\tst[pr]\tq} } } */
