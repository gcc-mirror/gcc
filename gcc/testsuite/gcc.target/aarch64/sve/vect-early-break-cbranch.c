/* { dg-do compile } */
/* { dg-options "-O3 -fno-schedule-insns -fno-reorder-blocks -fno-schedule-insns2" } */
/* { dg-final { check-function-bodies "**" "" "" { target lp64 } } } */
#define N 640
int a[N] = {0};
int b[N] = {0};
/*
** f1:
**	...
**	cmpgt	p[0-9]+.s, p[0-9]+/z, z[0-9]+.s, #0
**	ptest	p[0-9]+, p[0-9]+.b
**	b.(any|none)	\.L[0-9]+
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
**	cmpge	p[0-9]+.s, p[0-9]+/z, z[0-9]+.s, #0
**	ptest	p[0-9]+, p[0-9]+.b
**	b.(any|none)	\.L[0-9]+
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
**	cmpeq	p[0-9]+.s, p[0-9]+/z, z[0-9]+.s, #0
**	ptest	p[0-9]+, p[0-9]+.b
**	b.(any|none)	\.L[0-9]+
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
**	cmpne	p[0-9]+.s, p[0-9]+/z, z[0-9]+.s, #0
**	ptest	p[0-9]+, p[0-9]+.b
**	b.(any|none)	\.L[0-9]+
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
**	cmplt	p[0-9]+.s, p7/z, z[0-9]+.s, #0
**	ptest	p[0-9]+, p[0-9]+.b
**	b.(any|none)	.L[0-9]+
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
**	cmple	p[0-9]+.s, p[0-9]+/z, z[0-9]+.s, #0
**	ptest	p[0-9]+, p[0-9]+.b
**	b.(any|none)	\.L[0-9]+
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
