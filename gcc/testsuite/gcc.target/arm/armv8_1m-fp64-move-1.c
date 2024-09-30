/* { dg-do compile } */
/* { dg-options "-O" } */
/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-mfloat-abi=hard" } */
/* { dg-final { check-function-bodies "**" "" } } */

/*
** r_w:
**	vmov	r0, r1, d0
**	bx	lr
*/
void
r_w (double d0)
{
  register double r0 asm ("r0");
  r0 = d0;
  asm volatile ("" :: "r" (r0));
}

/*
** w_r:
**	vmov	d0, r0, r1
**	bx	lr
*/
double
w_r ()
{
  register double r0 asm ("r0");
  asm volatile ("" : "=r" (r0));
  return r0;
}

/*
** w_w:
**	vmov.f64	d1, d0
**	bx	lr
*/
void
w_w (double d0)
{
  register double d1 asm ("d1");
  d1 = d0;
  asm volatile ("" :: "w" (d1));
}

/*
** r_m_m32:
**	sub	(r[0-9]+), r0, #256
**	ldrd	r2, \[\1\]
**	bx	lr
*/
void
r_m_m32 (double *r0)
{
  register double r2 asm ("r2");
  r2 = r0[-32];
  asm volatile ("" :: "r" (r2));
}

/*
** r_m_m31:
**	ldrd	r2, \[r0, #-248\]
**	bx	lr
*/
void
r_m_m31 (double *r0)
{
  register double r2 asm ("r2");
  r2 = r0[-31];
  asm volatile ("" :: "r" (r2));
}

/*
** r_m_m1:
**	ldrd	r2, \[r0, #-8\]
**	bx	lr
*/
void
r_m_m1 (double *r0)
{
  register double r2 asm ("r2");
  r2 = r0[-1];
  asm volatile ("" :: "r" (r2));
}

/*
** r_m_0:
**	ldrd	r2, \[r0\]
**	bx	lr
*/
void
r_m_0 (double *r0)
{
  register double r2 asm ("r2");
  r2 = r0[0];
  asm volatile ("" :: "r" (r2));
}

/*
** r_m_1:
**	ldrd	r2, \[r0, #8\]
**	bx	lr
*/
void
r_m_1 (double *r0)
{
  register double r2 asm ("r2");
  r2 = r0[1];
  asm volatile ("" :: "r" (r2));
}

/*
** r_m_127:
**	ldrd	r2, \[r0, #1016\]
**	bx	lr
*/
void
r_m_127 (double *r0)
{
  register double r2 asm ("r2");
  r2 = r0[127];
  asm volatile ("" :: "r" (r2));
}

/*
** r_m_128:
**	add	(r[0-9]+), r0, #1024
**	ldrd	r2, \[r0\]
**	bx	lr
*/
void
r_m_128 (double *r0)
{
  register double r2 asm ("r2");
  r2 = r0[128];
  asm volatile ("" :: "r" (r2));
}

/* ??? This could be done in one instruction, but without mve.fp,
   it makes more sense for memory_operand to enforce the GPR range.  */
/*
** w_m_m32:
**	sub	(r[0-9]+), r0, #256
**	vldr.64	d0, \[\1\]
**	bx	lr
*/
void
w_m_m32 (double *r0)
{
  register double d0 asm ("d0");
  d0 = r0[-32];
  asm volatile ("" :: "w" (d0));
}

/*
** w_m_m31:
**	vldr.64	d0, \[r0, #-248\]
**	bx	lr
*/
void
w_m_m31 (double *r0)
{
  register double d0 asm ("d0");
  d0 = r0[-31];
  asm volatile ("" :: "w" (d0));
}

/*
** w_m_m1:
**	vldr.64	d0, \[r0, #-8\]
**	bx	lr
*/
void
w_m_m1 (double *r0)
{
  register double d0 asm ("d0");
  d0 = r0[-1];
  asm volatile ("" :: "w" (d0));
}

/*
** w_m_0:
**	vldr.64	d0, \[r0\]
**	bx	lr
*/
void
w_m_0 (double *r0)
{
  register double d0 asm ("d0");
  d0 = r0[0];
  asm volatile ("" :: "w" (d0));
}

/*
** w_m_1:
**	vldr.64	d0, \[r0, #8\]
**	bx	lr
*/
void
w_m_1 (double *r0)
{
  register double d0 asm ("d0");
  d0 = r0[1];
  asm volatile ("" :: "w" (d0));
}

/*
** w_m_127:
**	vldr.64	d0, \[r0, #1016\]
**	bx	lr
*/
void
w_m_127 (double *r0)
{
  register double d0 asm ("d0");
  d0 = r0[127];
  asm volatile ("" :: "w" (d0));
}

/*
** w_m_128:
**	add	(r[0-9]+), r0, #1024
**	vldr.64	d0, \[\1\]
**	bx	lr
*/
void
w_m_128 (double *r0)
{
  register double d0 asm ("d0");
  d0 = r0[128];
  asm volatile ("" :: "w" (d0));
}

/*
** m_m32_r:
**	sub	(r[0-9]+), r0, #256
**	strd	r2, \[\1\]
**	bx	lr
*/
void
m_m32_r (double *r0)
{
  register double r2 asm ("r2");
  asm volatile ("" : "=r" (r2));
  r0[-32] = r2;
}

/*
** m_m31_r:
**	strd	r2, \[r0, #-248\]
**	bx	lr
*/
void
m_m31_r (double *r0)
{
  register double r2 asm ("r2");
  asm volatile ("" : "=r" (r2));
  r0[-31] = r2;
}

/*
** m_m1_r:
**	strd	r2, \[r0, #-8\]
**	bx	lr
*/
void
m_m1_r (double *r0)
{
  register double r2 asm ("r2");
  asm volatile ("" : "=r" (r2));
  r0[-1] = r2;
}

/*
** m_0_r:
**	strd	r2, \[r0\]
**	bx	lr
*/
void
m_0_r (double *r0)
{
  register double r2 asm ("r2");
  asm volatile ("" : "=r" (r2));
  r0[0] = r2;
}

/*
** m_1_r:
**	strd	r2, \[r0, #8\]
**	bx	lr
*/
void
m_1_r (double *r0)
{
  register double r2 asm ("r2");
  asm volatile ("" : "=r" (r2));
  r0[1] = r2;
}

/*
** m_127_r:
**	strd	r2, \[r0, #1016\]
**	bx	lr
*/
void
m_127_r (double *r0)
{
  register double r2 asm ("r2");
  asm volatile ("" : "=r" (r2));
  r0[127] = r2;
}

/*
** m_128_r:
**	add	(r[0-9]+), r0, #1024
**	strd	r2, \[r0\]
**	bx	lr
*/
void
m_128_r (double *r0)
{
  register double r2 asm ("r2");
  asm volatile ("" : "=r" (r2));
  r0[128] = r2;
}

/* ??? This could be done in one instruction, but without mve.fp,
   it makes more sense for memory_operand to enforce the GPR range.  */
/*
** m_m32_w:
**	sub	(r[0-9]+), r0, #256
**	vstr.64	d0, \[\1\]
**	bx	lr
*/
void
m_m32_w (double *r0)
{
  register double d0 asm ("d0");
  asm volatile ("" : "=w" (d0));
  r0[-32] = d0;
}

/*
** m_m31_w:
**	vstr.64	d0, \[r0, #-248\]
**	bx	lr
*/
void
m_m31_w (double *r0)
{
  register double d0 asm ("d0");
  asm volatile ("" : "=w" (d0));
  r0[-31] = d0;
}

/*
** m_m1_w:
**	vstr.64	d0, \[r0, #-8\]
**	bx	lr
*/
void
m_m1_w (double *r0)
{
  register double d0 asm ("d0");
  asm volatile ("" : "=w" (d0));
  r0[-1] = d0;
}

/*
** m_0_w:
**	vstr.64	d0, \[r0\]
**	bx	lr
*/
void
m_0_w (double *r0)
{
  register double d0 asm ("d0");
  asm volatile ("" : "=w" (d0));
  r0[0] = d0;
}

/*
** m_1_w:
**	vstr.64	d0, \[r0, #8\]
**	bx	lr
*/
void
m_1_w (double *r0)
{
  register double d0 asm ("d0");
  asm volatile ("" : "=w" (d0));
  r0[1] = d0;
}

/*
** m_127_w:
**	vstr.64	d0, \[r0, #1016\]
**	bx	lr
*/
void
m_127_w (double *r0)
{
  register double d0 asm ("d0");
  asm volatile ("" : "=w" (d0));
  r0[127] = d0;
}

/*
** m_128_w:
**	add	(r[0-9]+), r0, #1024
**	vstr.64	d0, \[\1\]
**	bx	lr
*/
void
m_128_w (double *r0)
{
  register double d0 asm ("d0");
  asm volatile ("" : "=w" (d0));
  r0[128] = d0;
}
