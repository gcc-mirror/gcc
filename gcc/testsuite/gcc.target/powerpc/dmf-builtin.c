/* { dg-do compile } */
/* { dg-require-effective-target powerpc_future_compile_ok } */
/* { dg-require-effective-target lp64 } */
/* { dg-options "-mdejagnu-cpu=future -O2" } */

typedef unsigned char vec_t __attribute__((vector_size(16)));

void
foo (__dmr1024 *dst, __vector_pair *vpp, vec_t *src)
{
  __dmr1024 dmr;
  __vector_pair vp = *vpp;
  vec_t vec = *src;
  __builtin_dmsetdmrz (&dmr);
  __builtin_mma_dmxvi8gerx4 (&dmr, vp, vec);
  *dst = dmr;
}

void
bar (__dmr1024 *dst, __vector_pair *vpp, vec_t *src)
{
  __dmr1024 dmr = dst[0];
  __vector_pair vp = *vpp;
  vec_t vec = *src;
  __builtin_mma_dmxvi8gerx4 (&dmr, vp, vec);
  dst[1] = dmr;
}

/* { dg-final { scan-assembler-times {\mdmxvi8gerx4\M} 2 } } */

void
foo_1 (__dmr1024 *dst, __vector_pair *vpp, vec_t *src)
{
  __dmr1024 dmr;
  __vector_pair vp = *vpp;
  vec_t vec = *src;
  __builtin_dmsetdmrz (&dmr);
  __builtin_mma_dmxvi8gerx4pp (&dmr, vp, vec);
  *dst = dmr;
}

void
bar_1 (__dmr1024 *dst, __vector_pair *vpp, vec_t *src)
{
  __dmr1024 dmr = dst[0];;
  __vector_pair vp = *vpp;
  vec_t vec = *src;
  __builtin_mma_dmxvi8gerx4pp (&dmr, vp, vec);
  dst[1] = dmr;
}

/* { dg-final { scan-assembler-times {\mdmxvi8gerx4pp\M} 2 } } */

void
foo_2 (__dmr1024 *dst, __vector_pair *vpp, vec_t *src)
{
  __vector_pair vp = *vpp;
  vec_t vec = *src;
  __builtin_mma_pmdmxvi8gerx4 (dst, vp, vec, 255, 15, 2);
}

/* { dg-final { scan-assembler-times {\mpmdmxvi8gerx4\M} 1 } } */

void
foo_3 (__dmr1024 *dst, __vector_pair *vpp, vec_t *src)
{
  __vector_pair vp = *vpp;
  vec_t vec = *src;
  __builtin_mma_pmdmxvi8gerx4pp (dst, vp, vec, 255, 15, 2);
}

/* { dg-final { scan-assembler-times {\mpmdmxvi8gerx4pp\M} 1 } } */


void
foo_5 (__dmr1024 *dst, __dmr1024 *src)
{
  __builtin_dmxor (dst, src);
}

/* { dg-final { scan-assembler-times {\mdmxor\M} 1 } } */
