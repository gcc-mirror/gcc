/* { dg-do compile } */
/* { dg-require-effective-target vect_early_break } */
/* { dg-require-effective-target arm_arch_v8a_hard_ok } */
/* { dg-options "-O3 -fno-schedule-insns -fno-reorder-blocks -fno-schedule-insns2" } */
/* { dg-add-options arm_arch_v8a_hard } */
/* { dg-final { check-function-bodies "**" "" "" } } */

#define N 640
int a[N] = {0};
int b[N] = {0};

/*
** f1:
**	...
**	vcgt.s32	q[0-9]+, q[0-9]+, #0
**	vpmax.u32	d[0-9]+, d[0-9]+, d[0-9]+
**	vpmax.u32	d[0-9]+, d[0-9]+, d[0-9]+
**	vmov	r[0-9]+, s[0-9]+	@ int
** (
**	cmp	r[0-9]+, #0
**	bne	\.L[0-9]+
** |
**	cbn?z	r[0-9]+, \.L.+
** )
**	...
*/
void f1 ()
{
  for (int i = 0; i < N; i++)
    {
      b[i] += a[i];
      if (a[i] > 0)
	break;
    }
}

/*
** f2:
**	...
**	vcge.s32	q[0-9]+, q[0-9]+, #0
**	vpmax.u32	d[0-9]+, d[0-9]+, d[0-9]+
**	vpmax.u32	d[0-9]+, d[0-9]+, d[0-9]+
**	vmov	r[0-9]+, s[0-9]+	@ int
** (
**	cmp	r[0-9]+, #0
**	bne	\.L[0-9]+
** |
**	cbn?z	r[0-9]+, \.L.+
** )
**	...
*/
void f2 ()
{
  for (int i = 0; i < N; i++)
    {
      b[i] += a[i];
      if (a[i] >= 0)
	break;
    }
}

/*
** f3:
**	...
**	vceq.i32	q[0-9]+, q[0-9]+, #0
**	vpmax.u32	d[0-9]+, d[0-9]+, d[0-9]+
**	vpmax.u32	d[0-9]+, d[0-9]+, d[0-9]+
**	vmov	r[0-9]+, s[0-9]+	@ int
** (
**	cmp	r[0-9]+, #0
**	bne	\.L[0-9]+
** |
**	cbn?z	r[0-9]+, \.L.+
** )
**	...
*/
void f3 ()
{
  for (int i = 0; i < N; i++)
    {
      b[i] += a[i];
      if (a[i] == 0)
	break;
    }
}

/*
** f4:
**	...
**	vceq.i32	q[0-9]+, q[0-9]+, #0
**	vmvn	q[0-9]+, q[0-9]+
**	vpmax.u32	d[0-9]+, d[0-9]+, d[0-9]+
**	vpmax.u32	d[0-9]+, d[0-9]+, d[0-9]+
**	vmov	r[0-9]+, s[0-9]+	@ int
** (
**	cmp	r[0-9]+, #0
**	bne	\.L[0-9]+
** |
**	cbn?z	r[0-9]+, \.L.+
** )
**	...
*/
void f4 ()
{
  for (int i = 0; i < N; i++)
    {
      b[i] += a[i];
      if (a[i] != 0)
	break;
    }
}

/*
** f5:
**	...
**	vclt.s32	q[0-9]+, q[0-9]+, #0
**	vpmax.u32	d[0-9]+, d[0-9]+, d[0-9]+
**	vpmax.u32	d[0-9]+, d[0-9]+, d[0-9]+
**	vmov	r[0-9]+, s[0-9]+	@ int
** (
**	cmp	r[0-9]+, #0
**	bne	\.L[0-9]+
** |
**	cbn?z	r[0-9]+, \.L.+
** )
**	...
*/
void f5 ()
{
  for (int i = 0; i < N; i++)
    {
      b[i] += a[i];
      if (a[i] < 0)
	break;
    }
}

/*
** f6:
**	...
**	vcle.s32	q[0-9]+, q[0-9]+, #0
**	vpmax.u32	d[0-9]+, d[0-9]+, d[0-9]+
**	vpmax.u32	d[0-9]+, d[0-9]+, d[0-9]+
**	vmov	r[0-9]+, s[0-9]+	@ int
** (
**	cmp	r[0-9]+, #0
**	bne	\.L[0-9]+
** |
**	cbn?z	r[0-9]+, \.L.+
** )
**	...
*/
void f6 ()
{
  for (int i = 0; i < N; i++)
    {
      b[i] += a[i];
      if (a[i] <= 0)
	break;
    }
}

