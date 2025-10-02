/* { dg-do compile { target lp64 } } */
/* { dg-options "-O2 -march=z13 -mzarch" } */
/* { dg-final { check-function-bodies "**" "" } } */

typedef unsigned char uv16qi __attribute__ ((vector_size (16)));
typedef signed char v16qi __attribute__ ((vector_size (16)));
typedef unsigned short uv8hi __attribute__ ((vector_size (16)));
typedef signed short v8hi __attribute__ ((vector_size (16)));
typedef unsigned int uv4si __attribute__ ((vector_size (16)));
typedef signed int v4si __attribute__ ((vector_size (16)));
typedef unsigned long uv2di __attribute__ ((vector_size (16)));
typedef signed long v2di __attribute__ ((vector_size (16)));
typedef float v4sf __attribute__ ((vector_size (16)));
typedef double v2df __attribute__ ((vector_size (16)));

/*
** extractnthuchar:
**	vlgvb	%r2,%v24,3\(%r2\)
**	br	%r14
*/
unsigned char
extractnthuchar (uv16qi in, int n)
{
  return in[n + 3];
}

/*
** extractnthchar:
**	vlgvb	%r2,%v24,3\(%r2\)
**	lgbr	%r2,%r2
**	br	%r14
*/
signed char
extractnthchar (v16qi in, int n)
{
  return in[n + 3];
}

/*
** extractnthushort:
**	vlgvh	%r2,%v24,3\(%r2\)
**	br	%r14
*/
unsigned short
extractnthushort (uv8hi in, int n)
{
  return in[n + 3];
}

/*
** extractnthshort:
**	vlgvh	%r2,%v24,3\(%r2\)
**	lghr	%r2,%r2
**	br	%r14
*/
short
extractnthshort (v8hi in, int n)
{
  return in[n + 3];
}

/*
** extractnthuint:
**	vlgvf	%r2,%v24,3\(%r2\)
**	br	%r14
*/
unsigned int
extractnthuint (uv4si in, int n)
{
  return in[n + 3];
}

/*
** extractnthint:
**	vlgvf	%r2,%v24,3\(%r2\)
**	lgfr	%r2,%r2
**	br	%r14
*/
int
extractnthint (v4si in, int n)
{
  return in[n + 3];
}

/*
** extractnthulong:
**	vlgvg	%r2,%v24,1\(%r2\)
**	br	%r14
*/
unsigned long
extractnthulong (uv2di in, int n)
{
  return in[n + 1];
}

/*
** extractnthlong:
**	vlgvg	%r2,%v24,1\(%r2\)
**	br	%r14
*/
long
extractnthlong (v2di in, int n)
{
  return in[n + 1];
}

/*
** extractnthfloat:
**	vlgvf	%r1,%v24,3\(%r2\)
**	vlvgf	%v0,%r1,0
**	br	%r14
*/
float
extractnthfloat (v4sf in, int n)
{
  return in[n + 3];
}

/*
** extractnthdouble:
**	vlgvg	%r1,%v24,1\(%r2\)
**	ldgr	%f0,%r1
**	br	%r14
*/
double
extractnthdouble (v2df in, int n)
{
  return in[n + 1];
}

/*
** extractnthuintm1displacement:
**	ahi	%r2,-1
**	vlgvf	%r2,%v24,0\(%r2\)
**	br	%r14
*/
unsigned int
extractnthuintm1displacement (uv4si in, int n)
{
  return in[n - 1];
}
