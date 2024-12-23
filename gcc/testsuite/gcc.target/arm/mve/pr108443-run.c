/* { dg-do run } */
/* { dg-options "-O2" } */
/* { dg-require-effective-target arm_mve_hw } */
/* { dg-add-options arm_v8_1m_mve } */
#include "pr108443.c"

extern void abort (void);

void __attribute__ ((noipa)) partial_write_cst (uint32_t *, uint32x4_t);

void
__attribute__ ((noipa)) partial_write (uint32_t *a, uint32x4_t v, unsigned short p)
{
  vstrwq_p_u32 (a, v, p);
}

int main (void)
{
  unsigned short p = 0x00CC;
  uint32_t a[] = {0, 0, 0, 0};
  uint32_t b[] = {0, 0, 0, 0};
  uint32x4_t v = vdupq_n_u32 (0xFFFFFFFFU);
  partial_write_cst (&a[0], v);
  partial_write (&b[0], v, p);
  if (__builtin_memcmp (&a[0], &b[0], 16) != 0)
    abort ();

  return 0;
}
