
/* { dg-do compile } */
/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-options "-O3 -save-temps" } */
/* { dg-add-options arm_v8_1m_mve } */

#include <arm_mve.h>

/* We don't support pattern recognition of signed N values when computing num_iter.  */
void test3 (uint8_t *a, uint8_t *b, uint8_t *c, int n)
{
    int num_iter = (n + 15)/16;
    for (int i = 0; i < num_iter; i++)
    {
	mve_pred16_t p = vctp8q (n);
	uint8x16_t va = vldrbq_z_u8 (a, p);
	uint8x16_t vb = vldrbq_z_u8 (b, p);
	uint8x16_t vc = vaddq_x_u8 (va, vb, p);
	vstrbq_p_u8 (c, vc, p);
	n-=16;
	a += 16;
	b += 16;
	c += 16;
    }
}

/* Using a predicated vcmp to generate a new predicate value in the
   loop and then using it in a predicated store insn.  */
void test17 (int32_t *a, int32_t *b, int32_t *c, int n)
{
  while (n > 0)
    {
      mve_pred16_t p = vctp32q (n);
      int32x4_t va = vldrwq_z_s32 (a, p);
      int32x4_t vb = vldrwq_z_s32 (b, p);
      int32x4_t vc = vaddq_s32 (va, vb);
      mve_pred16_t p1 = vcmpeqq_m_s32 (va, vc, p);
      vstrwq_p_s32 (c, vc, p1);
      c += 4;
      a += 4;
      b += 4;
      n -= 4;
    }
}
/* This is an example of a loop that we could tail predicate but currently don't.  */
/* { dg-final { scan-assembler "letp" { xfail *-*-* } } } */
