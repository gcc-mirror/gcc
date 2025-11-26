/* { dg-do compile } */
/* { dg-options "-O3 -fno-schedule-insns -fno-reorder-blocks -fno-schedule-insns2" } */
/* { dg-final { check-function-bodies "**" "" "" { target lp64 } } } */

#pragma GCC target "+nosve"

#define N 640
unsigned int a[N] = {0};
unsigned int b[N] = {0};


/*
** f1:
**	...
**	cmhi	v[0-9]+.4s, v[0-9]+.4s, v[0-9]+.4s
**	umaxp	v[0-9]+.4s, v[0-9]+.4s, v[0-9]+.4s
**	fmov	x[0-9]+, d[0-9]+
**	cbn?z	x[0-9]+, \.L[0-9]+
**	...
*/
void f1 ()
{
  for (int i = 0; i < N; i++)
    {
      b[i] += a[i];
      if (a[i] > 1)
	break;
    }
}

/*
** f2:
**	...
**	cmtst	v[0-9]+.4s, v[0-9]+.4s, v[0-9]+.4s
**	umaxp	v[0-9]+.4s, v[0-9]+.4s, v[0-9]+.4s
**	fmov	x[0-9]+, d[0-9]+
**	cbn?z	x[0-9]+, \.L[0-9]+
**	...
*/
void f2 ()
{
  for (int i = 0; i < N; i++)
    {
      b[i] += a[i];
      if (a[i] >= 1)
	break;
    }
}

/*
** f3:
**	...
**	umaxp	v[0-9]+.4s, v[0-9]+.4s, v[0-9]+.4s
**	fmov	x[0-9]+, d[0-9]+
**	cbn?z	x[0-9]+, \.L[0-9]+
**	...
*/
void f3 ()
{
  for (int i = 0; i < N; i++)
    {
      b[i] += a[i];
      if (a[i] == 1)
	break;
    }
}

/*
** f4:
**	...
**	umaxp	v[0-9]+.4s, v[0-9]+.4s, v[0-9]+.4s
**	fmov	x[0-9]+, d[0-9]+
**	cbn?z	x[0-9]+, \.L[0-9]+
**	...
*/
void f4 ()
{
  for (int i = 0; i < N; i++)
    {
      b[i] += a[i];
      if (a[i] != 1)
	break;
    }
}

/*
** f5:
**	...
**	cmhs	v[0-9]+.4s, v[0-9]+.4s, v[0-9]+.4s
**	umaxp	v[0-9]+.4s, v[0-9]+.4s, v[0-9]+.4s
**	fmov	x[0-9]+, d[0-9]+
**	cbn?z	x[0-9]+, \.L[0-9]+
**	...
*/
void f5 ()
{
  for (int i = 0; i < N; i++)
    {
      b[i] += a[i];
      if (a[i] < 2)
	break;
    }
}

/*
** f6:
**	...
**	cmhs	v[0-9]+.4s, v[0-9]+.4s, v[0-9]+.4s
**	umaxp	v[0-9]+.4s, v[0-9]+.4s, v[0-9]+.4s
**	fmov	x[0-9]+, d[0-9]+
**	cbn?z	x[0-9]+, \.L[0-9]+
**	...
*/
void f6 ()
{
  for (int i = 0; i < N; i++)
    {
      b[i] += a[i];
      if (a[i] <= 2)
	break;
    }
}
