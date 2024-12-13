
/* { dg-do compile { target { arm*-*-* } } } */
/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-options "-O3 -save-temps -fno-schedule-insns2 " } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-mtune=cortex-m55" } */
/* { dg-final { check-function-bodies "**" "" "" } } */

#include <arm_mve.h>
/* Using a >=1 condition.  */
void test1 (int32_t *a, int32_t *b, int32_t *c, int n)
{
  while (n >= 1)
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
/*
** test1:
**...
**	dlstp.32	lr, r3
**	vldrw.32	q[0-9]+, \[r0\], #16
**	vldrw.32	q[0-9]+, \[r1\], #16
**	vadd.i32	(q[0-9]+), q[0-9]+, q[0-9]+
**	vstrw.32	\1, \[r2\], #16
**	letp	lr, .*
**...
*/

/* Test a for loop format of decrementing to zero */
int32_t a[] = {0, 1, 2, 3, 4, 5, 6, 7};
void test2 (int32_t *b, int num_elems)
{
    for (int i = num_elems; i > 0; i-= 4)
    {
        mve_pred16_t p = vctp32q (i);
        int32x4_t va = vldrwq_z_s32 (&(a[i]), p);
        vstrwq_p_s32 (b + i, va, p);
    }
}
/*
** test2:
**...
**	dlstp.32	lr, r1
**...
**	vldrw.32	(q[0-9]+), \[r3\], #-16
**	vstrw.32	\1, \[r0\], #-16
**	letp	lr, .*
**...
*/

/* Iteration counter counting up to num_iter.  */
void test3 (uint8_t *a, uint8_t *b, uint8_t *c, unsigned n)
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

/*
** test3:
**...
**	dlstp.8	lr, r3
**...
**	vldrb.8	q[0-9]+, \[(r[0-9]+|ip)\]
**	vldrb.8	q[0-9]+, \[(r[0-9]+|ip)\]
**...
**	vadd.i8	(q[0-9]+), q[0-9]+, q[0-9]+
**	vstrb.8	\3, \[(r[0-9]+|ip)\]
**...
**	letp	lr, .*
**...
*/

/* Iteration counter counting down from num_iter.  */
void test4 (uint8_t *a, uint8_t *b, uint8_t *c, int n)
{
    int num_iter = (n + 15)/16;
    for (int i = num_iter; i > 0; i--)
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
/*
** test4:
**...
**	dlstp.8	lr, r3
**...
**	vldrb.8	q[0-9]+, \[(r[0-9]+|ip)\]
**	vldrb.8	q[0-9]+, \[(r[0-9]+|ip)\]
**...
**	vadd.i8	(q[0-9]+), q[0-9]+, q[0-9]+
**	vstrb.8	\3, \[(r[0-9]+|ip)\]
**...
**	letp	lr, .*
**...
*/

/* Using an unpredicated arithmetic instruction within the loop.  */
void test5 (uint8_t *a, uint8_t *b, uint8_t *c,  uint8_t *d, int n)
{
    while (n > 0)
    {
        mve_pred16_t p = vctp8q (n);
        uint8x16_t va = vldrbq_z_u8 (a, p);
        uint8x16_t vb = vldrbq_u8 (b);
	/* Is affected by implicit predication, because vb also
	came from an unpredicated load, but there is no functional
	problem, because the result is used in a predicated store.  */
        uint8x16_t vc = vaddq_u8 (va, vb);
        uint8x16_t vd = vaddq_x_u8 (va, vb, p);
        vstrbq_p_u8 (c, vc, p);
        vstrbq_p_u8 (d, vd, p);
        n-=16;
	a += 16;
	b += 16;
	c += 16;
    }
}

/*
** test5:
**...
**	(?:mov	(r[0-9]+), r3)?
**...
**	dlstp.8	lr, (?:r[0-9]+|ip)
**...
**	vldrb.8	q[0-9]+, \[r1\]
**	vldrb.8	q[0-9]+, \[r2\]
**...
**	vadd.i8	(q[0-9]+), q[0-9]+, q[0-9]+
**...
**	vstrb.8	\2, \[r2\]
**	vstrb.8	\2, \[(r3|\1)\]
**	letp	lr, .*
**...
*/

/* Using a different VPR value for one instruction in the loop.  */
void test6 (int32_t *a, int32_t *b, int32_t *c, int n, mve_pred16_t p1)
{
  while (n > 0)
    {
      mve_pred16_t p = vctp32q (n);
      int32x4_t va = vldrwq_z_s32 (a, p);
      int32x4_t vb = vldrwq_z_s32 (b, p1);
      int32x4_t vc = vaddq_x_s32 (va, vb, p);
      vstrwq_p_s32 (c, vc, p);
      c += 4;
      a += 4;
      b += 4;
      n -= 4;
    }
}

/*
** test6:
**...
**	dlstp.32	lr, r3
**	vldrw.32	q[0-9]+, \[r0\], #16
**	vpst
**	vldrwt.32	q[0-9]+, \[r1\], #16
**	vadd.i32	(q[0-9]+), q[0-9]+, q[0-9]+
**	vstrw.32	\1, \[r2\], #16
**	letp	lr, .*
**...
*/

/* Generating and using another VPR value in the loop, with a vctp.
   The doloop logic will always try to do the transform on the first
   vctp it encounters, so this is still expected to work.  */
void test7 (int32_t *a, int32_t *b, int32_t *c, int n, int g)
{
  while (n > 0)
    {
      mve_pred16_t p = vctp32q (n);
      int32x4_t va = vldrwq_z_s32 (a, p);
      mve_pred16_t p1 = vctp32q (g);
      int32x4_t vb = vldrwq_z_s32 (b, p1);
      int32x4_t vc = vaddq_x_s32 (va, vb, p);
      vstrwq_p_s32 (c, vc, p);
      c += 4;
      a += 4;
      b += 4;
      n -= 4;
    }
}
/*
** test7:
**...
**	dlstp.32	lr, r3
**	vldrw.32	q[0-9]+, \[r0\], #16
** (
**	vmsr	p0, .*
**	vpst
** |
**	vpst
** )
**	vldrwt.32	q[0-9]+, \[r1\], #16
**	vadd.i32	(q[0-9]+), q[0-9]+, q[0-9]+
**	vstrw.32	\1, \[r2\], #16
**	letp	lr, .*
**...
*/

/* Generating and using a different VPR value in the loop, with a vctp,
   but this time the p1 will also change in every loop (still fine)  */
void test8 (int32_t *a, int32_t *b, int32_t *c, int n, int g)
{
  while (n > 0)
    {
      mve_pred16_t p = vctp32q (n);
      int32x4_t va = vldrwq_z_s32 (a, p);
      mve_pred16_t p1 = vctp32q (g);
      int32x4_t vb = vldrwq_z_s32 (b, p1);
      int32x4_t vc = vaddq_x_s32 (va, vb, p);
      vstrwq_p_s32 (c, vc, p);
      c += 4;
      a += 4;
      b += 4;
      n -= 4;
      g++;
    }
}

/*
** test8:
**...
**	dlstp.32	lr, r3
**	vldrw.32	q[0-9]+, \[r0\], #16
**	vctp.32	(?:r4|ip)
**	vpst
**	vldrwt.32	q[0-9]+, \[r1\], #16
**...
**	vadd.i32	(q[0-9]+), q[0-9]+, q[0-9]+
**	vstrw.32	\1, \[r2\], #16
**	letp	lr, .*
**...
*/

/* Generating and using a different VPR value in the loop, with a vctp_m
   that is independent of the loop vctp VPR.  */
void test9 (int32_t *a, int32_t *b, int32_t *c, int n, mve_pred16_t p1)
{
  while (n > 0)
    {
      mve_pred16_t p = vctp32q (n);
      int32x4_t va = vldrwq_z_s32 (a, p);
      mve_pred16_t p2 = vctp32q_m (n, p1);
      int32x4_t vb = vldrwq_z_s32 (b, p1);
      int32x4_t vc = vaddq_x_s32 (va, vb, p2);
      vstrwq_p_s32 (c, vc, p);
      c += 4;
      a += 4;
      b += 4;
      n -= 4;
    }
}

/*
** test9:
**...
**	dlstp.32	lr, r3
**	vldrw.32	q[0-9]+, \[r0\], #16
**	vmsr	p0, (r[0-9]+)	@ movhi
**	vpst
**	vctpt.32	r3
**	vmrs	(r[0-9]+), p0	@ movhi
**	vmsr	p0, \1	@ movhi
**	vpst
**	vldrwt.32	q[0-9]+, \[r1\], #16
**	vmsr	p0, \2	@ movhi
**	vpst
**	vaddt.i32	(q[0-9]+), q[0-9]+, q[0-9]+
**...
**	vstrw.32	\3, \[r2\], #16
**	letp	lr, .*
**...
*/

/* Generating and using a different VPR value in the loop,
   with a vctp_m that is tied to the base vctp VPR.  This
   is still fine, because the vctp_m will be transformed
   into a vctp and be implicitly predicated.  */
void test10 (int32_t *a, int32_t *b, int32_t *c, int n)
{
  while (n > 0)
    {
      mve_pred16_t p = vctp32q (n);
      int32x4_t va = vldrwq_z_s32 (a, p);
      mve_pred16_t p1 = vctp32q_m (n, p);
      int32x4_t vb = vldrwq_z_s32 (b, p1);
      int32x4_t vc = vaddq_x_s32 (va, vb, p1);
      vstrwq_p_s32 (c, vc, p);
      c += 4;
      a += 4;
      b += 4;
      n -= 4;
    }
}
/*
   We don't need that extra vctp in the loop, but we currently do not optimize
   it away, however, it is not wrong to use it...
*/
/*
** test10:
**...
**	dlstp.32	lr, r3
**	vctp.32	r3
**	vldrw.32	q[0-9]+, \[r0\], #16
**...
**	vpst
**	vldrwt.32	q[0-9]+, \[r1\], #16
**	vpst
**	vaddt.i32	(q[0-9]+), q[0-9]+, q[0-9]+
**	vstrw.32	\1, \[r2\], #16
**	letp	lr, .*
**...
*/

/* Generating and using a different VPR value in the loop, with a vcmp.  */
void test11 (int32_t *a, int32_t *b, int32_t *c, int n)
{
  while (n > 0)
    {
      mve_pred16_t p = vctp32q (n);
      int32x4_t va = vldrwq_z_s32 (a, p);
      int32x4_t vb = vldrwq_z_s32 (b, p);
      mve_pred16_t p1 = vcmpeqq_s32 (va, vb);
      int32x4_t vc = vaddq_x_s32 (va, vb, p1);
      vstrwq_p_s32 (c, vc, p);
      c += 4;
      a += 4;
      b += 4;
      n -= 4;
    }
}

/*
** test11:
**...
**	dlstp.32	lr, r3
**	vldrw.32	q[0-9]+, \[r0\], #16
**	vldrw.32	q[0-9]+, \[r1\], #16
**	vcmp.i32	eq, q[0-9]+, q[0-9]+
**	vpst
**	vaddt.i32	(q[0-9]+), q[0-9]+, q[0-9]+
**	vstrw.32	\1, \[r2\], #16
**	letp	lr, .*
**...
*/

/* Generating and using a different VPR value in the loop, with a vcmp_m.  */
void test12 (int32_t *a, int32_t *b, int32_t *c, int n, mve_pred16_t p1)
{
  while (n > 0)
    {
      mve_pred16_t p = vctp32q (n);
      int32x4_t va = vldrwq_z_s32 (a, p);
      int32x4_t vb = vldrwq_z_s32 (b, p);
      mve_pred16_t p2 = vcmpeqq_m_s32 (va, vb, p1);
      int32x4_t vc = vaddq_x_s32 (va, vb, p2);
      vstrwq_p_s32 (c, vc, p);
      c += 4;
      a += 4;
      b += 4;
      n -= 4;
    }
}

/*
** test12:
**...
**	dlstp.32	lr, r3
**	vldrw.32	q[0-9]+, \[r0\], #16
**	vldrw.32	q[0-9]+, \[r1\], #16
**	vmsr	p0, (r[0-9]+|ip)	@ movhi
**	vpst
**	vcmpt.i32	eq, q[0-9]+, q[0-9]+
**	vpst
**	vaddt.i32	(q[0-9]+), q[0-9]+, q[0-9]+
**	vstrw.32	\2, \[r2\], #16
**	letp	lr, .*
**...
*/

/* Generating and using a different VPR value in the loop, with a vcmp_m
   that is tied to the base vctp VPR (same as above, this will be turned
   into a vcmp and be implicitly predicated).  */
void test13 (int32_t *a, int32_t *b, int32_t *c, int n, mve_pred16_t p1)
{
  while (n > 0)
    {
      mve_pred16_t p = vctp32q (n);
      int32x4_t va = vldrwq_z_s32 (a, p);
      int32x4_t vb = vldrwq_z_s32 (b, p);
      mve_pred16_t p2 = vcmpeqq_m_s32 (va, vb, p);
      int32x4_t vc = vaddq_x_s32 (va, vb, p2);
      vstrwq_p_s32 (c, vc, p);
      c += 4;
      a += 4;
      b += 4;
      n -= 4;
    }
}

/*
** test13:
**...
**	dlstp.32	lr, r3
**	vldrw.32	q[0-9]+, \[r0\], #16
**	vldrw.32	q[0-9]+, \[r1\], #16
**	vcmp.i32	eq, q[0-9]+, q[0-9]+
**	vpst
**	vaddt.i32	(q[0-9]+), q[0-9]+, q[0-9]+
**	vstrw.32	\1, \[r2\], #16
**	letp	lr, .*
**...
*/

/* Similar to test27 in dsltp-invalid-asm.c, but use a predicated load to make
   it safe to implicitly predicate the vaddv.  */
void test14 (int32_t *a, int32_t *c, int n)
{
  int32_t res = 0;
  while (n > 0)
    {
      mve_pred16_t p = vctp32q (n);
      int32x4_t va = vldrwq_z_s32 (a, p);
      res += vaddvq_s32 (va);
      int32x4_t vc = vdupq_n_s32 (res);
      vstrwq_p_s32 (c, vc, p);
      a += 4;
      n -= 4;
    }
}

/*
** test14:
**...
**	dlstp.32	lr, r2
**	vldrw.32	(q[0-9]+), \[r0\], #16
**	vaddv.s32	(r[0-9]+|ip), \1
**	add	(r[0-9]+|ip), \3, \2
**	vdup.32	(q[0-9]+), \3
**	vstrw.32	\4, \[r1\]
**	letp	lr, .*
**...
*/

uint8_t test15 (uint8_t *a, uint8_t *b, int n)
{
    uint8_t res = 0;
    uint8x16_t vc = vdupq_n_u8 (0);
    while (n > 0)
    {
       mve_pred16_t p = vctp8q (n);
       uint8x16_t va = vldrbq_z_u8 (a, p);
       uint8x16_t vb = vldrbq_u8 (b);
       vc = vaddq_m (vc, va, vc, p);
       res = vgetq_lane (vc, 5);

       a += 16;
       b += 16;
       n -= 16;
    }
    return res;
}

/*
** test15:
**...
**	dlstp.8	lr, r2
**...
**	vldrb.8	q[0-9]+, \[(r[0-9]+|ip)\]
**...
**	vadd.i8	(q[0-9]+), q[0-9]+, q[0-9]+
**...
**	letp	lr, .*
**	vmov.u8	r[0-9]+, \2\[5\]
**...
*/

uint8_t test16 (uint8_t *a, uint8_t *b, int n)
{
    uint8_t res = 0;
    uint8x16_t vc = vdupq_n_u8 (0);
    while (n > 0)
    {
       mve_pred16_t p = vctp8q (n);
       uint8x16_t va = vldrbq_z_u8 (a, p);
       uint8x16_t vb = vldrbq_u8 (b);
       vc = vaddq (va, vc);
       vc = vaddq_m (vc, va, vc, p);
       res = vgetq_lane (vc, 5);

       a += 16;
       b += 16;
       n -= 16;
    }
    return res;
}

/*
** test16:
**...
**	dlstp.8	lr, r2
**...
**	vldrb.8	q[0-9]+, \[(r[0-9]+|ip)\]
**...
**	vadd.i8	(q[0-9]+), q[0-9]+, q[0-9]+
**	vadd.i8	\2, q[0-9]+, q[0-9]+
**	letp	lr, .*
**	vmov.u8	r[0-9]+, \2\[5\]
**...
*/



/* Using an across-vector unpredicated instruction in a valid way.
   This tests that "vc" has correctly masked the risky "vb".  */
uint16_t test18 (uint16_t *a, uint16_t *b,  uint16_t *c, int n)
{
  uint16x8_t vb = vldrhq_u16 (b);
  uint16_t res = 0;
  while (n > 0)
    {
      mve_pred16_t p = vctp16q (n);
      uint16x8_t va = vldrhq_z_u16 (a, p);
      uint16x8_t vc = vaddq_m_u16 (va, va, vb, p);
      res += vaddvq_u16 (vc);
      c += 8;
      a += 8;
      b += 8;
      n -= 8;
    }
  return res;
}

/*
** test18:
**...
**	dlstp.16	lr, r3
**	vldrh.16	(q[0-9]+), \[r2\], #16
**	vadd.i16	\1, q[0-9]+, q[0-9]+
**	vaddv.u16	(r[0-9]+|ip), \1
**	add	(r[0-9]+|ip), \3, \2
**	uxth	\3, \3
**	letp	lr, .*
**...
*/

/* Using an across-vector unpredicated instruction with implicit scalar adding from outside the loop.  */
uint16_t test19 (uint16_t *a, uint16_t *b,  uint16_t *c, int n)
{
  uint16x8_t vb = vldrhq_u16 (b);
  uint16_t res = 0;
  while (n > 0)
    {
      mve_pred16_t p = vctp16q (n);
      uint16x8_t va = vldrhq_z_u16 (a, p);
      uint16x8_t vc = vaddq_m_u16 (va, va, vb, p);
      res = vaddvaq_u16 (res, vc);
      c += 8;
      a += 8;
      b += 8;
      n -= 8;
    }
  return res;
}

/*
** test19:
**...
**	dlstp.16	lr, r3
**	vldrh.16	(q[0-9]+), \[r2\], #16
**	vadd.i16	\1, q[0-9]+, q[0-9]+
**	vaddva.u16	(r[0-9]+|ip), \1
**	uxth	\2, \2
**	letp	lr, .*
**...
*/


/* Using an across-vector predicated instruction in a valid way.  */
uint16_t  test20 (uint16_t *a, uint16_t *b,  uint16_t *c, int n)
{
  uint16_t res = 0;
  while (n > 0)
    {
      mve_pred16_t p = vctp16q (n);
      uint16x8_t va = vldrhq_u16 (a);
      res = vaddvaq_p_u16 (res, va, p);
      c += 8;
      a += 8;
      b += 8;
      n -= 8;
    }
  return res;
}

/* The uxth could be moved outside the loop.  */
/*
** test20:
**...
**	dlstp.16	lr, r3
**	vldrh.16	(q[0-9]+), \[r2\], #16
**	vaddva.u16	(r[0-9]+|ip), \1
**	uxth	\2, \2
**	letp	lr, .*
**...
*/

/* Using an across-vector predicated instruction in a valid way.  */
uint16_t  test21 (uint16_t *a, uint16_t *b,  uint16_t *c, int n)
{
  uint16_t res = 0;
  while (n > 0)
    {
      mve_pred16_t p = vctp16q (n);
      uint16x8_t va = vldrhq_u16 (a);
      res++;
      res = vaddvaq_p_u16 (res, va, p);
      c += 8;
      a += 8;
      b += 8;
      n -= 8;
    }
  return res;
}

/* Also think it'd be safe to move uxth outside of the loop here.  */
/*
** test21:
**...
**	dlstp.16	lr, r3
**	vldrh.16	(q[0-9]+), \[r2\], #16
**	adds	(r[0-9]+|ip), \2, #1
**	uxth	\2, \2
**	vaddva.u16	\2, \1
**	uxth	\2, \2
**	letp	lr, .*
**...
*/

int test22 (uint8_t *a, uint8_t *b, uint8_t *c, int n)
{
    int res = 0;
    while (n > 0)
    {
        mve_pred16_t p = vctp8q (n);
        uint8x16_t va = vldrbq_z_u8 (a, p);
        res = vmaxvq (res, va);
        n-=16;
        a+=16;
    }
    return res;
}

/*
** test22:
**...
**	dlstp.8	lr, r3
**...
**	vldrb.8	(q[0-9]+), \[r[0-9]+\]
**...
**	vmaxv.u8	(r[0-9]+|ip), \1
**	uxtb	\2, \2
**	letp	lr, .*
**...
*/

int test23 (int8_t *a, int8_t *b, int8_t *c, int n)
{
    int res = 0;
    while (n > 0)
    {
        mve_pred16_t p = vctp8q (n);
        int8x16_t va = vldrbq_z_s8 (a, p);
        res = vmaxavq (res, va);
        n-=16;
        a+=16;
    }
    return res;
}

/*
** test23:
**...
**	dlstp.8	lr, r3
**...
**	vldrb.8	(q[0-9]+), \[r3\]
**...
**	vmaxav.s8	(r[0-9]+|ip), \1
**	uxtb	\2, \2
**	letp	lr, .*
**...
*/

/* Like test1, but update n before vctp, meaning we should only iterate for n-4
   elements.  */
void test24 (int32_t *a, int32_t *b, int32_t *c, int n)
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
    }
}
/*
** test24:
**...
**	subs	r3, r3, #4
**...
**	dlstp.32	lr, r3
**	vldrw.32	q[0-9]+, \[r0\], #16
**	vldrw.32	q[0-9]+, \[r1\], #16
**	vadd.i32	(q[0-9]+), q[0-9]+, q[0-9]+
**	vstrw.32	\1, \[r2\], #16
**	letp	lr, .*
**...
*/

