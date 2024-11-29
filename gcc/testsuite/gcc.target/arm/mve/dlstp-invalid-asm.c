/* { dg-do compile { target { arm*-*-* } } } */
/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-options "-O3 -save-temps" } */
/* { dg-add-options arm_v8_1m_mve } */

#include <limits.h>
#include <arm_mve.h>

/* Terminating on a non-zero number of elements.  */
void test0 (uint8_t *a, uint8_t *b, uint8_t *c, int n)
{
    while (n > 1)
    {
       mve_pred16_t p = vctp8q (n);
       uint8x16_t va = vldrbq_z_u8 (a, p);
       uint8x16_t vb = vldrbq_z_u8 (b, p);
       uint8x16_t vc = vaddq_x_u8 (va, vb, p);
       vstrbq_p_u8 (c, vc, p);
       n -= 16;
    }
}

/* Terminating on n >= 0.  */
void test1 (uint8_t *a, uint8_t *b, uint8_t *c, int n)
{
    while (n >= 0)
    {
       mve_pred16_t p = vctp8q (n);
       uint8x16_t va = vldrbq_z_u8 (a, p);
       uint8x16_t vb = vldrbq_z_u8 (b, p);
       uint8x16_t vc = vaddq_x_u8 (va, vb, p);
       vstrbq_p_u8 (c, vc, p);
       n -= 16;
    }
}

/* Similar, terminating on a non-zero number of elements, but in a for loop
   format.  */
int32_t a[] = {0, 1, 2, 3, 4, 5, 6, 7};
void test2 (int32_t *b, int num_elems)
{
    for (int i = num_elems; i >= 2; i-= 4)
    {
       mve_pred16_t p = vctp32q (i);
       int32x4_t va = vldrwq_z_s32 (&(a[i]), p);
       vstrwq_p_s32 (b + i, va, p);
    }
}

/* Iteration counter counting up to num_iter, with a non-zero starting num.  */
void test3 (uint8_t *a, uint8_t *b, uint8_t *c, int n)
{
    int num_iter = (n + 15)/16;
    for (int i = 1; i < num_iter; i++)
    {
       mve_pred16_t p = vctp8q (n);
       uint8x16_t va = vldrbq_z_u8 (a, p);
       uint8x16_t vb = vldrbq_z_u8 (b, p);
       uint8x16_t vc = vaddq_x_u8 (va, vb, p);
       vstrbq_p_u8 (c, vc, p);
       n -= 16;
    }
}

/* Iteration counter counting up to num_iter, with a larger increment  */
void test4 (uint8_t *a, uint8_t *b, uint8_t *c, int n)
{
    int num_iter = (n + 15)/16;
    for (int i = 0; i < num_iter; i+=2)
    {
       mve_pred16_t p = vctp8q (n);
       uint8x16_t va = vldrbq_z_u8 (a, p);
       uint8x16_t vb = vldrbq_z_u8 (b, p);
       uint8x16_t vc = vaddq_x_u8 (va, vb, p);
       vstrbq_p_u8 (c, vc, p);
       n -= 16;
    }
}

/* Using an unpredicated store instruction within the loop.  */
void test5 (uint8_t *a, uint8_t *b, uint8_t *c,  uint8_t *d, int n)
{
    while (n > 0)
    {
       mve_pred16_t p = vctp8q (n);
       uint8x16_t va = vldrbq_z_u8 (a, p);
       uint8x16_t vb = vldrbq_z_u8 (b, p);
       uint8x16_t vc = vaddq_u8 (va, vb);
       uint8x16_t vd = vaddq_x_u8 (va, vb, p);
       vstrbq_u8 (d, vd);
       n -= 16;
    }
}

/* Using an unpredicated store outside the loop.  */
void test6 (uint8_t *a, uint8_t *b, uint8_t *c, int n, uint8x16_t vx)
{
    while (n > 0)
    {
       mve_pred16_t p = vctp8q (n);
       uint8x16_t va = vldrbq_z_u8 (a, p);
       uint8x16_t vb = vldrbq_z_u8 (b, p);
       uint8x16_t vc = vaddq_m_u8 (vx, va, vb, p);
       vx = vaddq_u8 (vx, vc);
       a += 16;
       b += 16;
       n -= 16;
    }
    vstrbq_u8 (c, vx);
}

/* Using a VPR that gets modified within the loop.  */
void test9 (int32_t *a, int32_t *b, int32_t *c, int n)
{
  while (n > 0)
    {
      mve_pred16_t p = vctp32q (n);
      int32x4_t va = vldrwq_z_s32 (a, p);
      p++;
      int32x4_t vb = vldrwq_z_s32 (b, p);
      int32x4_t vc = vaddq_x_s32 (va, vb, p);
      vstrwq_p_s32 (c, vc, p);
      c += 4;
      a += 4;
      b += 4;
      n -= 4;
    }
}

/* Using a VPR that gets re-generated within the loop.  Even though we
   currently reject such loops, it would be possible to dlstp transform this
   specific loop, as long as we make sure that the first vldrwq_z mask would
   either:
   * remain the same as its mask in the first iteration,
   * become the same as the loop mask after the first iteration,
   * become all ones, since the dlstp would then mask it the same as the loop
   mask.  */
void test10a (int32_t *a, int32_t *b, int32_t *c, int n)
{
  mve_pred16_t p = vctp32q (n);
  while (n > 0)
    {
      int32x4_t va = vldrwq_z_s32 (a, p);
      p = vctp32q (n);
      int32x4_t vb = vldrwq_z_s32 (b, p);
      int32x4_t vc = vaddq_x_s32 (va, vb, p);
      vstrwq_p_s32 (c, vc, p);
      c += 4;
      a += 4;
      b += 4;
      n -= 4;
    }
}

/* Using a VPR that gets re-generated within the loop, the difference between
   this test and test10a is to make sure the two vctp calls are never the same,
   this leads to slightly different codegen in some cases triggering the issue
   in a different way.   This loop too would be OK to dlstp transform as long
   as we made sure that the first vldrwq_z mask would either:
   * remain the same as the its mask in the first iteration,
   * become the same as the loop mask after the first iteration,
   * become all ones, since the dlstp would then mask it the same as the loop
   mask.  */
void test10b (int32_t *a, int32_t *b, int32_t *c, int n)
{
  mve_pred16_t p = vctp32q (n-4);
  while (n > 0)
    {
      int32x4_t va = vldrwq_z_s32 (a, p);
      p = vctp32q (n);
      int32x4_t vb = vldrwq_z_s32 (b, p);
      int32x4_t vc = vaddq_x_s32 (va, vb, p);
      vstrwq_p_s32 (c, vc, p);
      c += 4;
      a += 4;
      b += 4;
      n -= 4;
    }
}

/* Using vctp32q_m instead of vctp32q.  */
void test11 (int32_t *a, int32_t *b, int32_t *c, int n, mve_pred16_t p0)
{
  while (n > 0)
    {
      mve_pred16_t p = vctp32q_m (n, p0);
      int32x4_t va = vldrwq_z_s32 (a, p);
      int32x4_t vb = vldrwq_z_s32 (b, p);
      int32x4_t vc = vaddq_x_s32 (va, vb, p);
      vstrwq_p_s32 (c, vc, p);
      c += 4;
      a += 4;
      b += 4;
      n -= 4;
    }
}

/* Using an unpredicated op with a scalar output, where the result is valid
   outside the bb.  This is invalid, because one of the inputs to the
   unpredicated op is also unpredicated.  */
uint8_t test12 (uint8_t *a, uint8_t *b, uint8_t *c, int n, uint8x16_t vx)
{
    uint8_t sum = 0;
    while (n > 0)
    {
       mve_pred16_t p = vctp8q (n);
       uint8x16_t va = vldrbq_z_u8 (a, p);
       uint8x16_t vb = vldrbq_u8 (b);
       uint8x16_t vc = vaddq_u8 (va, vb);
       sum += vaddvq_u8 (vc);
       a += 16;
       b += 16;
       n -= 16;
    }
    return sum;
}

/* Using an unpredicated vcmp to generate a new predicate value in the
   loop and then using that VPR to predicate a store insn.  */
void test13 (int32_t *a, int32_t *b, int32x4_t vc, int32_t *c, int n)
{
  while (n > 0)
    {
      mve_pred16_t p = vctp32q (n);
      int32x4_t va = vldrwq_s32 (a);
      int32x4_t vb = vldrwq_z_s32 (b, p);
      int32x4_t vc = vaddq_s32 (va, vb);
      mve_pred16_t p1 = vcmpeqq_s32 (va, vc);
      vstrwq_p_s32 (c, vc, p1);
      c += 4;
      a += 4;
      b += 4;
      n -= 4;
    }
}

/* Using an across-vector unpredicated instruction. "vb" is the risk.  */
uint16_t test14 (uint16_t *a, uint16_t *b,  uint16_t *c, int n)
{
  uint16x8_t vb = vldrhq_u16 (b);
  uint16_t res = 0;
  while (n > 0)
    {
      mve_pred16_t p = vctp16q (n);
      uint16x8_t va = vldrhq_z_u16 (a, p);
      vb = vaddq_u16 (va, vb);
      res = vaddvq_u16 (vb);
      c += 8;
      a += 8;
      b += 8;
      n -= 8;
    }
  return res;
}

/* Using an across-vector unpredicated instruction. "vc" is the risk. */
uint16_t test15 (uint16_t *a, uint16_t *b,  uint16_t *c, int n)
{
  uint16x8_t vb = vldrhq_u16 (b);
  uint16_t res = 0;
  while (n > 0)
    {
      mve_pred16_t p = vctp16q (n);
      uint16x8_t va = vldrhq_z_u16 (a, p);
      uint16x8_t vc = vaddq_u16 (va, vb);
      res = vaddvaq_u16 (res, vc);
      c += 8;
      a += 8;
      b += 8;
      n -= 8;
    }
  return res;
}

uint16_t test16 (uint16_t *a, uint16_t *b,  uint16_t *c, int n)
{
  uint16_t res =0;
  while (n > 0)
    {
      mve_pred16_t p = vctp16q (n);
      uint16x8_t vb = vldrhq_u16 (b);
      uint16x8_t va = vldrhq_z_u16 (a, p);
      res = vaddvaq_u16 (res, vb);
      res = vaddvaq_p_u16 (res, va, p);
      c += 8;
      a += 8;
      b += 8;
      n -= 8;
    }
  return res;
}

int test17 (int8_t *a, int8_t *b, int8_t *c, int n)
{
    int res = 0;
    while (n > 0)
    {
        mve_pred16_t p = vctp8q (n);
        int8x16_t va = vldrbq_z_s8 (a, p);
        res = vmaxvq (res, va);
        n-=16;
        a+=16;
    }
    return res;
}



int test18 (int8_t *a, int8_t *b, int8_t *c, int n)
{
    int res = 0;
    while (n > 0)
    {
        mve_pred16_t p = vctp8q (n);
        int8x16_t va = vldrbq_z_s8 (a, p);
        res = vminvq (res, va);
        n-=16;
        a+=16;
    }
    return res;
}

int test19 (int8_t *a, int8_t *b, int8_t *c, int n)
{
    int res = 0;
    while (n > 0)
    {
        mve_pred16_t p = vctp8q (n);
        int8x16_t va = vldrbq_z_s8 (a, p);
        res = vminavq (res, va);
        n-=16;
        a+=16;
    }
    return res;
}

int test20 (uint8_t *a, uint8_t *b, uint8_t *c, int n)
{
    int res = 0;
    while (n > 0)
    {
        mve_pred16_t p = vctp8q (n);
        uint8x16_t va = vldrbq_z_u8 (a, p);
        res = vminvq (res, va);
        n-=16;
        a+=16;
    }
    return res;
}

uint8x16_t test21 (uint8_t *a, uint32_t *b, int n, uint8x16_t res)
{
    while (n > 0)
    {
        mve_pred16_t p = vctp8q (n);
        uint8x16_t va = vldrbq_z_u8 (a, p);
        res = vshlcq_u8 (va, b, 1);
        n-=16;
        a+=16;
    }
    return res;
}

int8x16_t test22 (int8_t *a, int32_t *b, int n, int8x16_t res)
{
    while (n > 0)
    {
        mve_pred16_t p = vctp8q (n);
        int8x16_t va = vldrbq_z_s8 (a, p);
        res = vshlcq_s8 (va, b, 1);
        n-=16;
        a+=16;
    }
    return res;
}

/* Using an unsigned number of elements to count down from, with a >0*/
void test23 (int32_t *a, int32_t *b, int32_t *c, unsigned int n)
{
  while (n > 0)
    {
      mve_pred16_t p = vctp32q (n);
      int32x4_t va = vldrwq_z_s32 (a, p);
      int32x4_t vb = vldrwq_z_s32 (b, p);
      int32x4_t vc = vaddq_x_s32 (va, vb, p);
      vstrwq_p_s32 (c, vc, p);
      c+=4;
      a+=4;
      b+=4;
      n-=4;
    }
}

/* Using an unsigned number of elements to count up to, with a <n*/
void test24 (uint8_t *a, uint8_t *b, uint8_t *c, unsigned int n)
{
    for (int i = 0; i < n; i+=16)
    {
        mve_pred16_t p = vctp8q (n-i);
        uint8x16_t va = vldrbq_z_u8 (a, p);
        uint8x16_t vb = vldrbq_z_u8 (b, p);
        uint8x16_t vc = vaddq_x_u8 (va, vb, p);
        vstrbq_p_u8 (c, vc, p);
        n-=16;
    }
}


/* Using an unsigned number of elements to count up to, with a <=n*/
void test25 (uint8_t *a, uint8_t *b, uint8_t *c, unsigned int n)
{
    for (int i = 1; i <= n; i+=16)
    {
        mve_pred16_t p = vctp8q (n-i+1);
        uint8x16_t va = vldrbq_z_u8 (a, p);
        uint8x16_t vb = vldrbq_z_u8 (b, p);
        uint8x16_t vc = vaddq_x_u8 (va, vb, p);
        vstrbq_p_u8 (c, vc, p);
        n-=16;
    }
}
/* Update n twice in the loop.  */
void test26 (int32_t *a, int32_t *b, int32_t *c, int n)
{
  while (n >= 1)
    {
      n-=4;
      mve_pred16_t p = vctp32q (n);
      int32x4_t va = vldrwq_z_s32 (a, p);
      int32x4_t vb = vldrwq_z_s32 (b, p);
      int32x4_t vc = vaddq_x_s32 (va, vb, p);
      vstrwq_p_s32 (c, vc, p);
      c+=4;
      a+=4;
      b+=4;
      n-=4;
    }
}

void test27 (int32_t *a, int32_t *c, int n)
{
  int32_t res = 0;
  while (n > 0)
    {
      mve_pred16_t p = vctp32q (n);
      int32x4_t va = vldrwq_s32 (a);
      res += vaddvq_s32 (va);
      int32x4_t vc = vdupq_n_s32 (res);
      vstrwq_p_s32 (c, vc, p);
      a += 4;
      n -= 4;
    }
}

/* Using an unpredicated vcmp to generate a new predicate value in the
   loop and then using it in a predicated store insn.  */
void test28 (int32_t *a, int32_t *b, int32_t *c, int n)
{
  while (n > 0)
    {
      mve_pred16_t p = vctp32q (n);
      int32x4_t va = vldrwq_z_s32 (a, p);
      int32x4_t vb = vldrwq_s32 (b);
      int32x4_t vc = vaddq_x_s32 (va, vb, p);
      mve_pred16_t p1 = vcmpeqq_s32 (va, vc);
      vstrwq_p_s32 (c, vc, p1);
      c += 4;
      a += 4;
      b += 4;
      n -= 4;
    }
}

/* Using an unpredicated op with a scalar output, where the result is valid
   outside the bb.  The unpredicated lanes are not guaranteed zero, so would
   affect the vaddv in the non-tail predicated case.  */
uint8_t test29 (uint8_t *a, uint8_t *b, uint8_t *c, int n, uint8x16_t vx)
{
    uint8_t sum = 0;
    while (n > 0)
    {
       mve_pred16_t p = vctp8q (n);
       uint8x16_t va = vldrbq_z_u8 (a, p);
       uint8x16_t vb = vldrbq_z_u8 (b, p);
       uint8x16_t vc = vaddq_m_u8 (vx, va, vb, p);
       sum += vaddvq_u8 (vc);
       a += 16;
       b += 16;
       n -= 16;
    }
    return sum;
}

/* Same as above, but with another scalar op between the unpredicated op and
   the scalar op outside the loop.  */
uint8_t test30 (uint8_t *a, uint8_t *b, uint8_t *c, int n, uint8x16_t vx, int g)
{
    uint8_t sum = 0;
    while (n > 0)
    {
       mve_pred16_t p = vctp8q (n);
       uint8x16_t va = vldrbq_z_u8 (a, p);
       uint8x16_t vb = vldrbq_z_u8 (b, p);
       uint8x16_t vc = vaddq_m_u8 (vx, va, vb, p);
       sum += vaddvq_u8 (vc);
       sum += g;
       a += 16;
       b += 16;
       n -= 16;
    }
    return sum;
}

uint8_t test31 (uint8_t *a, uint8_t *b, int n)
{
    uint8_t res = 0;
    uint8x16_t vc = vdupq_n_u8 (0);
    while (n > 0)
    {
       mve_pred16_t p = vctp8q (n);
       uint8x16_t va = vldrbq_z_u8 (a, p);
       uint8x16_t vb = vldrbq_u8 (b);
       vc = vaddq (vb, vc);
       res = vgetq_lane (vc, 5);

       a += 16;
       b += 16;
       n -= 16;
    }
    return res;
}

uint8_t test32 (uint8_t *a, uint8_t *b, int n)
{
    uint8_t res = 0;
    uint8x16_t vc = vdupq_n_u8 (0);
    while (n > 0)
    {
       mve_pred16_t p = vctp8q (n);
       uint8x16_t va = vldrbq_z_u8 (a, p);
       uint8x16_t vb = vldrbq_u8 (b);
       vc = vaddq_m (vc, va, vc, p);
       vc = vaddq (vb, vc);
       res = vgetq_lane (vc, 5);

       a += 16;
       b += 16;
       n -= 16;
    }
    return res;
}

/* { dg-final { scan-assembler-not "\tdlstp" } } */
/* { dg-final { scan-assembler-not "\tletp" } } */
