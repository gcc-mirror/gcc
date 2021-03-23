/* { dg-do compile } */
/* { dg-options "-O -mfp16-format=ieee" } */
/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-mfloat-abi=hard" } *
/* { dg-final { check-function-bodies "**" "" } } */

/*
** r_w:
**	vmov.f16	r0, s0	@ __fp16
**	bx	lr
*/
void
r_w (_Float16 s0)
{
  register _Float16 r0 asm ("r0");
  r0 = s0;
  asm volatile ("" :: "r" (r0));
}

/*
** w_r:
**	vmov.f16	s0, r0	@ __fp16
**	bx	lr
*/
_Float16
w_r ()
{
  register _Float16 r0 asm ("r0");
  asm volatile ("" : "=r" (r0));
  return r0;
}

/*
** w_w:
**	vmov	s1, s0	@ __fp16
**	bx	lr
*/
void
w_w (_Float16 s0)
{
  register _Float16 s1 asm ("s1");
  s1 = s0;
  asm volatile ("" :: "w" (s1));
}

/*
** r_m_m128:
**	sub	(r[0-9]+), r0, #256
**	ldrh	r1, \[\1\]	@ __fp16
**	bx	lr
*/
void
r_m_m128 (_Float16 *r0)
{
  register _Float16 r1 asm ("r1");
  r1 = r0[-128];
  asm volatile ("" :: "r" (r1));
}

/*
** r_m_m127:
**	ldrh	r1, \[r0, #-254\]	@ __fp16
**	bx	lr
*/
void
r_m_m127 (_Float16 *r0)
{
  register _Float16 r1 asm ("r1");
  r1 = r0[-127];
  asm volatile ("" :: "r" (r1));
}

/*
** r_m_m1:
**	ldrh	r1, \[r0, #-2\]	@ __fp16
**	bx	lr
*/
void
r_m_m1 (_Float16 *r0)
{
  register _Float16 r1 asm ("r1");
  r1 = r0[-1];
  asm volatile ("" :: "r" (r1));
}

/*
** r_m_0:
**	ldrh	r1, \[r0\]	@ __fp16
**	bx	lr
*/
void
r_m_0 (_Float16 *r0)
{
  register _Float16 r1 asm ("r1");
  r1 = r0[0];
  asm volatile ("" :: "r" (r1));
}

/*
** r_m_1:
**	ldrh	r1, \[r0, #2\]	@ __fp16
**	bx	lr
*/
void
r_m_1 (_Float16 *r0)
{
  register _Float16 r1 asm ("r1");
  r1 = r0[1];
  asm volatile ("" :: "r" (r1));
}

/*
** r_m_255:
**	ldrh	r1, \[r0, #510\]	@ __fp16
**	bx	lr
*/
void
r_m_255 (_Float16 *r0)
{
  register _Float16 r1 asm ("r1");
  r1 = r0[255];
  asm volatile ("" :: "r" (r1));
}

/*
** r_m_256:
**	ldrh	r1, \[r0, #512\]	@ __fp16
**	bx	lr
*/
void
r_m_256 (_Float16 *r0)
{
  register _Float16 r1 asm ("r1");
  r1 = r0[256];
  asm volatile ("" :: "r" (r1));
}

/* ??? This could be done in one instruction, but without mve.fp,
   it makes more sense for memory_operand to enforce the GPR range.  */
/*
** w_m_m128:
**	sub	(r[0-9]+), r0, #256
**	vldr.16	s0, \[\1\]
**	bx	lr
*/
void
w_m_m128 (_Float16 *r0)
{
  register _Float16 s0 asm ("s0");
  s0 = r0[-128];
  asm volatile ("" :: "w" (s0));
}

/*
** w_m_m127:
**	vldr.16	s0, \[r0, #-254\]
**	bx	lr
*/
void
w_m_m127 (_Float16 *r0)
{
  register _Float16 s0 asm ("s0");
  s0 = r0[-127];
  asm volatile ("" :: "w" (s0));
}

/*
** w_m_m1:
**	vldr.16	s0, \[r0, #-2\]
**	bx	lr
*/
void
w_m_m1 (_Float16 *r0)
{
  register _Float16 s0 asm ("s0");
  s0 = r0[-1];
  asm volatile ("" :: "w" (s0));
}

/*
** w_m_0:
**	vldr.16	s0, \[r0\]
**	bx	lr
*/
void
w_m_0 (_Float16 *r0)
{
  register _Float16 s0 asm ("s0");
  s0 = r0[0];
  asm volatile ("" :: "w" (s0));
}

/*
** w_m_1:
**	vldr.16	s0, \[r0, #2\]
**	bx	lr
*/
void
w_m_1 (_Float16 *r0)
{
  register _Float16 s0 asm ("s0");
  s0 = r0[1];
  asm volatile ("" :: "w" (s0));
}

/*
** w_m_255:
**	vldr.16	s0, \[r0, #510\]
**	bx	lr
*/
void
w_m_255 (_Float16 *r0)
{
  register _Float16 s0 asm ("s0");
  s0 = r0[255];
  asm volatile ("" :: "w" (s0));
}

/*
** w_m_256:
**	add	(r[0-9]+), r0, #512
**	vldr.16	s0, \[\1\]
**	bx	lr
*/
void
w_m_256 (_Float16 *r0)
{
  register _Float16 s0 asm ("s0");
  s0 = r0[256];
  asm volatile ("" :: "w" (s0));
}

/*
** m_m128_r:
**	sub	(r[0-9]+), r0, #256
**	strh	r1, \[\1\]	@ __fp16
**	bx	lr
*/
void
m_m128_r (_Float16 *r0)
{
  register _Float16 r1 asm ("r1");
  asm volatile ("" : "=r" (r1));
  r0[-128] = r1;
}

/*
** m_m127_r:
**	strh	r1, \[r0, #-254\]	@ __fp16
**	bx	lr
*/
void
m_m127_r (_Float16 *r0)
{
  register _Float16 r1 asm ("r1");
  asm volatile ("" : "=r" (r1));
  r0[-127] = r1;
}

/*
** m_m1_r:
**	strh	r1, \[r0, #-2\]	@ __fp16
**	bx	lr
*/
void
m_m1_r (_Float16 *r0)
{
  register _Float16 r1 asm ("r1");
  asm volatile ("" : "=r" (r1));
  r0[-1] = r1;
}

/*
** m_0_r:
**	strh	r1, \[r0\]	@ __fp16
**	bx	lr
*/
void
m_0_r (_Float16 *r0)
{
  register _Float16 r1 asm ("r1");
  asm volatile ("" : "=r" (r1));
  r0[0] = r1;
}

/*
** m_1_r:
**	strh	r1, \[r0, #2\]	@ __fp16
**	bx	lr
*/
void
m_1_r (_Float16 *r0)
{
  register _Float16 r1 asm ("r1");
  asm volatile ("" : "=r" (r1));
  r0[1] = r1;
}

/*
** m_255_r:
**	strh	r1, \[r0, #510\]	@ __fp16
**	bx	lr
*/
void
m_255_r (_Float16 *r0)
{
  register _Float16 r1 asm ("r1");
  asm volatile ("" : "=r" (r1));
  r0[255] = r1;
}

/*
** m_256_r:
**	strh	r1, \[r0, #512\]	@ __fp16
**	bx	lr
*/
void
m_256_r (_Float16 *r0)
{
  register _Float16 r1 asm ("r1");
  asm volatile ("" : "=r" (r1));
  r0[256] = r1;
}

/* ??? This could be done in one instruction, but without mve.fp,
   it makes more sense for memory_operand to enforce the GPR range.  */
/*
** m_m128_w:
**	sub	(r[0-9]+), r0, #256
**	vstr.16	s0, \[\1\]
**	bx	lr
*/
void
m_m128_w (_Float16 *r0)
{
  register _Float16 s0 asm ("s0");
  asm volatile ("" : "=w" (s0));
  r0[-128] = s0;
}

/*
** m_m127_w:
**	vstr.16	s0, \[r0, #-254\]
**	bx	lr
*/
void
m_m127_w (_Float16 *r0)
{
  register _Float16 s0 asm ("s0");
  asm volatile ("" : "=w" (s0));
  r0[-127] = s0;
}

/*
** m_m1_w:
**	vstr.16	s0, \[r0, #-2\]
**	bx	lr
*/
void
m_m1_w (_Float16 *r0)
{
  register _Float16 s0 asm ("s0");
  asm volatile ("" : "=w" (s0));
  r0[-1] = s0;
}

/*
** m_0_w:
**	vstr.16	s0, \[r0\]
**	bx	lr
*/
void
m_0_w (_Float16 *r0)
{
  register _Float16 s0 asm ("s0");
  asm volatile ("" : "=w" (s0));
  r0[0] = s0;
}

/*
** m_1_w:
**	vstr.16	s0, \[r0, #2\]
**	bx	lr
*/
void
m_1_w (_Float16 *r0)
{
  register _Float16 s0 asm ("s0");
  asm volatile ("" : "=w" (s0));
  r0[1] = s0;
}

/*
** m_255_w:
**	vstr.16	s0, \[r0, #510\]
**	bx	lr
*/
void
m_255_w (_Float16 *r0)
{
  register _Float16 s0 asm ("s0");
  asm volatile ("" : "=w" (s0));
  r0[255] = s0;
}

/*
** m_256_w:
**	add	(r[0-9]+), r0, #512
**	vstr.16	s0, \[\1\]
**	bx	lr
*/
void
m_256_w (_Float16 *r0)
{
  register _Float16 s0 asm ("s0");
  asm volatile ("" : "=w" (s0));
  r0[256] = s0;
}
