/* { dg-do compile } */
/* { dg-options "-O" } */
/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-mfloat-abi=hard" } *
/* { dg-final { check-function-bodies "**" "" } } */

/*
** r_w:
**	vmov	r0, s0
**	bx	lr
*/
void
r_w (float s0)
{
  register float r0 asm ("r0");
  r0 = s0;
  asm volatile ("" :: "r" (r0));
}

/*
** w_r:
**	vmov	s0, r0
**	bx	lr
*/
float
w_r ()
{
  register float r0 asm ("r0");
  asm volatile ("" : "=r" (r0));
  return r0;
}

/*
** w_w:
**	vmov.f32	s1, s0
**	bx	lr
*/
void
w_w (float s0)
{
  register float s1 asm ("s1");
  s1 = s0;
  asm volatile ("" :: "w" (s1));
}

/*
** r_m_m64:
**	sub	(r[0-9]+), r0, #256
**	ldr	r1, \[\1\]	@ float
**	bx	lr
*/
void
r_m_m64 (float *r0)
{
  register float r1 asm ("r1");
  r1 = r0[-64];
  asm volatile ("" :: "r" (r1));
}

/*
** r_m_m63:
**	ldr	r1, \[r0, #-252\]	@ float
**	bx	lr
*/
void
r_m_m63 (float *r0)
{
  register float r1 asm ("r1");
  r1 = r0[-63];
  asm volatile ("" :: "r" (r1));
}

/*
** r_m_m1:
**	ldr	r1, \[r0, #-4\]	@ float
**	bx	lr
*/
void
r_m_m1 (float *r0)
{
  register float r1 asm ("r1");
  r1 = r0[-1];
  asm volatile ("" :: "r" (r1));
}

/*
** r_m_0:
**	ldr	r1, \[r0\]	@ float
**	bx	lr
*/
void
r_m_0 (float *r0)
{
  register float r1 asm ("r1");
  r1 = r0[0];
  asm volatile ("" :: "r" (r1));
}

/*
** r_m_1:
**	ldr	r1, \[r0, #4\]	@ float
**	bx	lr
*/
void
r_m_1 (float *r0)
{
  register float r1 asm ("r1");
  r1 = r0[1];
  asm volatile ("" :: "r" (r1));
}

/*
** r_m_255:
**	ldr	r1, \[r0, #1020\]	@ float
**	bx	lr
*/
void
r_m_255 (float *r0)
{
  register float r1 asm ("r1");
  r1 = r0[255];
  asm volatile ("" :: "r" (r1));
}

/*
** r_m_256:
**	add	(r[0-9]+), r0, #1024
**	ldr	r1, \[r0\]	@ float
**	bx	lr
*/
void
r_m_256 (float *r0)
{
  register float r1 asm ("r1");
  r1 = r0[256];
  asm volatile ("" :: "r" (r1));
}

/* ??? This could be done in one instruction, but without mve.fp,
   it makes more sense for memory_operand to enforce the GPR range.  */
/*
** w_m_m64:
**	sub	(r[0-9]+), r0, #256
**	vldr.32	s0, \[\1\]
**	bx	lr
*/
void
w_m_m64 (float *r0)
{
  register float s0 asm ("s0");
  s0 = r0[-64];
  asm volatile ("" :: "w" (s0));
}

/*
** w_m_m63:
**	vldr.32	s0, \[r0, #-252\]
**	bx	lr
*/
void
w_m_m63 (float *r0)
{
  register float s0 asm ("s0");
  s0 = r0[-63];
  asm volatile ("" :: "w" (s0));
}

/*
** w_m_m1:
**	vldr.32	s0, \[r0, #-4\]
**	bx	lr
*/
void
w_m_m1 (float *r0)
{
  register float s0 asm ("s0");
  s0 = r0[-1];
  asm volatile ("" :: "w" (s0));
}

/*
** w_m_0:
**	vldr.32	s0, \[r0\]
**	bx	lr
*/
void
w_m_0 (float *r0)
{
  register float s0 asm ("s0");
  s0 = r0[0];
  asm volatile ("" :: "w" (s0));
}

/*
** w_m_1:
**	vldr.32	s0, \[r0, #4\]
**	bx	lr
*/
void
w_m_1 (float *r0)
{
  register float s0 asm ("s0");
  s0 = r0[1];
  asm volatile ("" :: "w" (s0));
}

/*
** w_m_255:
**	vldr.32	s0, \[r0, #1020\]
**	bx	lr
*/
void
w_m_255 (float *r0)
{
  register float s0 asm ("s0");
  s0 = r0[255];
  asm volatile ("" :: "w" (s0));
}

/*
** w_m_256:
**	add	(r[0-9]+), r0, #1024
**	vldr.32	s0, \[\1\]
**	bx	lr
*/
void
w_m_256 (float *r0)
{
  register float s0 asm ("s0");
  s0 = r0[256];
  asm volatile ("" :: "w" (s0));
}

/*
** m_m64_r:
**	sub	(r[0-9]+), r0, #256
**	str	r1, \[\1\]	@ float
**	bx	lr
*/
void
m_m64_r (float *r0)
{
  register float r1 asm ("r1");
  asm volatile ("" : "=r" (r1));
  r0[-64] = r1;
}

/*
** m_m63_r:
**	str	r1, \[r0, #-252\]	@ float
**	bx	lr
*/
void
m_m63_r (float *r0)
{
  register float r1 asm ("r1");
  asm volatile ("" : "=r" (r1));
  r0[-63] = r1;
}

/*
** m_m1_r:
**	str	r1, \[r0, #-4\]	@ float
**	bx	lr
*/
void
m_m1_r (float *r0)
{
  register float r1 asm ("r1");
  asm volatile ("" : "=r" (r1));
  r0[-1] = r1;
}

/*
** m_0_r:
**	str	r1, \[r0\]	@ float
**	bx	lr
*/
void
m_0_r (float *r0)
{
  register float r1 asm ("r1");
  asm volatile ("" : "=r" (r1));
  r0[0] = r1;
}

/*
** m_1_r:
**	str	r1, \[r0, #4\]	@ float
**	bx	lr
*/
void
m_1_r (float *r0)
{
  register float r1 asm ("r1");
  asm volatile ("" : "=r" (r1));
  r0[1] = r1;
}

/*
** m_255_r:
**	str	r1, \[r0, #1020\]	@ float
**	bx	lr
*/
void
m_255_r (float *r0)
{
  register float r1 asm ("r1");
  asm volatile ("" : "=r" (r1));
  r0[255] = r1;
}

/*
** m_256_r:
**	add	(r[0-9]+), r0, #1024
**	str	r1, \[r0\]	@ float
**	bx	lr
*/
void
m_256_r (float *r0)
{
  register float r1 asm ("r1");
  asm volatile ("" : "=r" (r1));
  r0[256] = r1;
}

/* ??? This could be done in one instruction, but without mve.fp,
   it makes more sense for memory_operand to enforce the GPR range.  */
/*
** m_m64_w:
**	sub	(r[0-9]+), r0, #256
**	vstr.32	s0, \[\1\]
**	bx	lr
*/
void
m_m64_w (float *r0)
{
  register float s0 asm ("s0");
  asm volatile ("" : "=w" (s0));
  r0[-64] = s0;
}

/*
** m_m63_w:
**	vstr.32	s0, \[r0, #-252\]
**	bx	lr
*/
void
m_m63_w (float *r0)
{
  register float s0 asm ("s0");
  asm volatile ("" : "=w" (s0));
  r0[-63] = s0;
}

/*
** m_m1_w:
**	vstr.32	s0, \[r0, #-4\]
**	bx	lr
*/
void
m_m1_w (float *r0)
{
  register float s0 asm ("s0");
  asm volatile ("" : "=w" (s0));
  r0[-1] = s0;
}

/*
** m_0_w:
**	vstr.32	s0, \[r0\]
**	bx	lr
*/
void
m_0_w (float *r0)
{
  register float s0 asm ("s0");
  asm volatile ("" : "=w" (s0));
  r0[0] = s0;
}

/*
** m_1_w:
**	vstr.32	s0, \[r0, #4\]
**	bx	lr
*/
void
m_1_w (float *r0)
{
  register float s0 asm ("s0");
  asm volatile ("" : "=w" (s0));
  r0[1] = s0;
}

/*
** m_255_w:
**	vstr.32	s0, \[r0, #1020\]
**	bx	lr
*/
void
m_255_w (float *r0)
{
  register float s0 asm ("s0");
  asm volatile ("" : "=w" (s0));
  r0[255] = s0;
}

/*
** m_256_w:
**	add	(r[0-9]+), r0, #1024
**	vstr.32	s0, \[\1\]
**	bx	lr
*/
void
m_256_w (float *r0)
{
  register float s0 asm ("s0");
  asm volatile ("" : "=w" (s0));
  r0[256] = s0;
}
