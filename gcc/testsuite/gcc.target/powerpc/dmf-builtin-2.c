/* { dg-do compile } */
/* { dg-require-effective-target powerpc_future_compile_ok } */
/* { dg-options "-mdejagnu-cpu=future -O2" } */

typedef unsigned char vec_t __attribute__((vector_size(16)));

void
foo (__dmr1024 *dst, __vector_pair *vpp, vec_t *src)
{
  __dmr1024 dmr;
  __vector_pair vp = *vpp;
  vec_t vec = *src;
  __builtin_dmsetdmrz (&dmr);
  __builtin_mma_dmxvf16gerx2 (&dmr, vp, vec);
  *dst = dmr;
}

void
bar (__dmr1024 *dst, __vector_pair *vpp, vec_t *src)
{
  __dmr1024 dmr = dst[0];;
  __vector_pair vp = *vpp;
  vec_t vec = *src;
  __builtin_mma_dmxvf16gerx2 (&dmr, vp, vec);
  dst[1] = dmr;
}

/* { dg-final { scan-assembler-times {\mdmxvf16gerx2\M} 2 } } */

void
foo_1 (__dmr1024 *dst, __vector_pair *vpp, vec_t *src)
{
  __dmr1024 dmr;
  __vector_pair vp = *vpp;
  vec_t vec = *src;
  __builtin_dmsetdmrz (&dmr);
  __builtin_mma_dmxvf16gerx2nn (&dmr, vp, vec);
  *dst = dmr;
}

void
bar_1 (__dmr1024 *dst, __vector_pair *vpp, vec_t *src)
{
  __dmr1024 dmr = dst[0];;
  __vector_pair vp = *vpp;
  vec_t vec = *src;
  __builtin_mma_dmxvf16gerx2nn (&dmr, vp, vec);
  dst[1] = dmr;
}

/* { dg-final { scan-assembler-times {\mdmxvf16gerx2nn\M} 2 } } */

void
foo_2 (__dmr1024 *dst, __vector_pair *vpp, vec_t *src)
{
  __dmr1024 dmr;
  __vector_pair vp = *vpp;
  vec_t vec = *src;
  __builtin_dmsetdmrz (&dmr);
  __builtin_mma_dmxvf16gerx2np (&dmr, vp, vec);
  *dst = dmr;
}

void
bar_2 (__dmr1024 *dst, __vector_pair *vpp, vec_t *src)
{
  __dmr1024 dmr = dst[0];;
  __vector_pair vp = *vpp;
  vec_t vec = *src;
  __builtin_mma_dmxvf16gerx2np (&dmr, vp, vec);
  dst[1] = dmr;
}

/* { dg-final { scan-assembler-times {\mdmxvf16gerx2np\M} 2 } } */

void
foo_3 (__dmr1024 *dst, __vector_pair *vpp, vec_t *src)
{
  __dmr1024 dmr;
  __vector_pair vp = *vpp;
  vec_t vec = *src;
  __builtin_dmsetdmrz (&dmr);
  __builtin_mma_dmxvf16gerx2pn (&dmr, vp, vec);
  *dst = dmr;
}

void
bar_3 (__dmr1024 *dst, __vector_pair *vpp, vec_t *src)
{
  __dmr1024 dmr = dst[0];;
  __vector_pair vp = *vpp;
  vec_t vec = *src;
  __builtin_mma_dmxvf16gerx2pn (&dmr, vp, vec);
  dst[1] = dmr;
}

/* { dg-final { scan-assembler-times {\mdmxvf16gerx2pn\M} 2 } } */

void
foo_4 (__dmr1024 *dst, __vector_pair *vpp, vec_t *src)
{
  __dmr1024 dmr;
  __vector_pair vp = *vpp;
  vec_t vec = *src;
  __builtin_dmsetdmrz (&dmr);
  __builtin_mma_dmxvf16gerx2pp (&dmr, vp, vec);
  *dst = dmr;
}

void
bar_4 (__dmr1024 *dst, __vector_pair *vpp, vec_t *src)
{
  __dmr1024 dmr = dst[0];;
  __vector_pair vp = *vpp;
  vec_t vec = *src;
  __builtin_mma_dmxvf16gerx2pp (&dmr, vp, vec);
  dst[1] = dmr;
}

/* { dg-final { scan-assembler-times {\mdmxvf16gerx2pp\M} 2 } } */

void
foo_5 (__dmr1024 *dst, __vector_pair *vpp, vec_t *src)
{
  __vector_pair vp = *vpp;
  vec_t vec = *src;
  __builtin_mma_pmdmxvf16gerx2 (dst, vp, vec, 255, 15, 2);
}

/* { dg-final { scan-assembler-times {\mpmdmxvf16gerx2\M} 1 } } */

void
foo_6 (__dmr1024 *dst, __vector_pair *vpp, vec_t *src)
{
  __vector_pair vp = *vpp;
  vec_t vec = *src;
  __builtin_mma_pmdmxvf16gerx2nn (dst, vp, vec, 255, 15, 2);
}

/* { dg-final { scan-assembler-times {\mpmdmxvf16gerx2nn\M} 1 } } */

void
foo_7 (__dmr1024 *dst, __vector_pair *vpp, vec_t *src)
{
  __vector_pair vp = *vpp;
  vec_t vec = *src;
  __builtin_mma_pmdmxvf16gerx2np (dst, vp, vec, 255, 15, 2);
}

/* { dg-final { scan-assembler-times {\mpmdmxvf16gerx2np\M} 1 } } */

void
foo_8 (__dmr1024 *dst, __vector_pair *vpp, vec_t *src)
{
  __vector_pair vp = *vpp;
  vec_t vec = *src;
  __builtin_mma_pmdmxvf16gerx2pn (dst, vp, vec, 255, 15, 2);
}

/* { dg-final { scan-assembler-times {\mpmdmxvf16gerx2pn\M} 1 } } */

void
foo_9 (__dmr1024 *dst, __vector_pair *vpp, vec_t *src)
{
  __vector_pair vp = *vpp;
  vec_t vec = *src;
  __builtin_mma_pmdmxvf16gerx2pp (dst, vp, vec, 255, 15, 2);
}

/* { dg-final { scan-assembler-times {\mpmdmxvf16gerx2pp\M} 1 } } */
