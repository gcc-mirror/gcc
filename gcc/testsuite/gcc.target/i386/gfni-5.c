/* { dg-do compile } */
/* { dg-options "-O2 -msse2 -mgfni" } */

typedef char __attribute__((vector_size(16))) v16qi_t;

v16qi_t test16a (v16qi_t x, v16qi_t a)
{
  asm volatile ("" : "+m" (a));
  return __builtin_ia32_vgf2p8affineqb_v16qi (x, a, 0);
}

v16qi_t test16b (v16qi_t x, v16qi_t a)
{
  asm volatile ("" : "+m" (x));
  return __builtin_ia32_vgf2p8affineqb_v16qi (x, a, 0);
}

/* { dg-final { scan-assembler-times "gf2p8affineqb\[ \t].*\\(" 1 } } */
/* { dg-final { scan-assembler-times "gf2p8affineqb\[ \t].*%xmm.*%xmm" 1 } } */
