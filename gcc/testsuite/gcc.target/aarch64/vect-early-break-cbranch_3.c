/* { dg-do compile } */
/* { dg-options "-O3 -fno-schedule-insns -fno-reorder-blocks -fno-schedule-insns2 --param aarch64-autovec-preference=asimd-only" } */
/* { dg-final { check-function-bodies "**" "" "" { target lp64 } } } */

#pragma GCC target "+sve"

#define N 640
float a[N] = {0};
float b[N] = {0};

/*
** f1:
**	...
**	fcmgt	p[0-9]+.s, p[0-9]+/z, z[0-9]+.s, #0.0
**	ptest	p[0-9]+, p[0-9]+\.b
**	b(\.?eq|\.none)	\.L[0-9]+
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
**	fcmge	p[0-9]+.s, p[0-9]+/z, z[0-9]+.s, #0.0
**	ptest	p[0-9]+, p[0-9]+\.b
**	b(\.?eq|\.none)	\.L[0-9]+
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
**	fcmeq	p[0-9]+.s, p[0-9]+/z, z[0-9]+.s, #0.0
**	ptest	p[0-9]+, p[0-9]+\.b
**	b(\.?eq|\.none)	\.L[0-9]+
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
**	fcmne	p[0-9]+.s, p[0-9]+/z, z[0-9]+.s, #0.0
**	ptest	p[0-9]+, p[0-9]+\.b
**	b(\.?eq|\.none)	\.L[0-9]+
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
**	fcmlt	p[0-9]+.s, p7/z, z[0-9]+.s, #0.0
**	ptest	p[0-9]+, p[0-9]+\.b
**	b(\.?eq|\.none)	.L[0-9]+
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
**	fcmle	p[0-9]+.s, p[0-9]+/z, z[0-9]+.s, #0.0
**	ptest	p[0-9]+, p[0-9]+\.b
**	b(\.?eq|\.none)	\.L[0-9]+
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
