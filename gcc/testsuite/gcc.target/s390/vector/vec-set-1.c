/* { dg-do compile } */
/* { dg-options "-O2 -march=z14 -mzarch" } */
/* { dg-final { check-function-bodies "**" "" } } */

typedef double V2DF __attribute__((vector_size(16)));
typedef double V1DF __attribute__((vector_size(8)));
typedef float V4SF __attribute__((vector_size(16)));

/*
** setdf0:
**	vpdi	%v24,%v0,%v24,1
**	br	%r14
*/
V2DF
setdf0 (V2DF x, double y)
{
  x[0] = y;
  return x;
}

/*
** setdf1:
**	vmrhg	%v24,%v24,%v0
**	br	%r14
*/
V2DF
setdf1 (V2DF x, double y)
{
  x[1] = y;
  return x;
}

/*
** setdfn:
**	lgdr	(%r.),%f0
**	vlvgg	%v24,\1,0\(%r2\)
**	br	%r14
*/
V2DF
setdfn (V2DF x, double y, int n)
{
  x[n] = y;
  return x;
}

/*
** set1df:
**	vlr	%v24,%v0
**	br	%r14
*/
V1DF
set1df (V1DF x, double y)
{
  x[0] = y;
  return x;
}

/*
** set1dfn:
**	vlr	%v24,%v0
**	br	%r14
*/
V1DF
set1dfn (V1DF x, double y, int n)
{
  x[n] = y;
  return x;
}

/*
** setsf0:
**	lgdr	(%r.),%f0
**	vlvgf	%v24,\1,0
**	br	%r14
*/
V4SF
setsf0 (V4SF x, float y)
{
  x[0] = y;
  return x;
}

/*
** setsf1:
**	lgdr	(%r.),%f0
**	vlvgf	%v24,\1,1
**	br	%r14
*/
V4SF
setsf1 (V4SF x, float y)
{
  x[1] = y;
  return x;
}

/*
** setsf0:
**	lgdr	(%r.),%f0
**	vlvgf	%v24,\1,2
**	br	%r14
*/
V4SF
setsf2 (V4SF x, float y)
{
  x[2] = y;
  return x;
}

/*
** setsf1:
**	lgdr	(%r.),%f0
**	vlvgf	%v24,\1,3
**	br	%r14
*/
V4SF
setsf3 (V4SF x, float y)
{
  x[3] = y;
  return x;
}

/*
** setsfn:
**	lgdr	(%r.),%f0
**	vlvgf	%v24,\1,0\(%r2\)
**	br	%r14
*/
V4SF
setsfn (V4SF x, float y, int n)
{
  x[n] = y;
  return x;
}
