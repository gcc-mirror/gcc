/* { dg-do compile } */
/* { dg-options "-O2 -march=z14 -mzarch" } */
/* { dg-final { check-function-bodies "**" "" } } */

typedef double V2DF __attribute__((vector_size(16)));
typedef float V4SF __attribute__((vector_size(16)));
typedef float V2SF __attribute__((vector_size(8)));
typedef double V1DF __attribute__((vector_size(8)));
typedef float V1SF __attribute__((vector_size(4)));
typedef long double V1TF __attribute__((vector_size(16)));

/*
** extractfirstdouble:
**	vlr	%v0,%v24
**	br	%r14
*/
double
extractfirstdouble (V2DF x)
{
  return x[0];
}

/*
** extractseconddouble:
**	vrepg	%v0,%v24,1
**	br	%r14
*/
double
extractseconddouble (V2DF x)
{
  return x[1];
}

/*
** extractnthdouble:
**	vlgvg	(%r.),%v24,0\(%r2\)
**	ldgr	%f0,\1
**	br	%r14
*/
double
extractnthdouble (V2DF x, int n)
{
  return x[n];
}

/*
** sumfirstdouble:
**	vfadb	%v0,%v24,%v26
**	br	%r14
*/
double
sumfirstdouble (V2DF x, V2DF y)
{
  return (x + y)[0];
}

/*
** extractfirstfloat:
**	vlr	%v0,%v24
**	br	%r14
*/
float
extractfirstfloat (V4SF x)
{
  return x[0];
}

/*
** extractsecondfloat:
**	vrepf	%v0,%v24,1
**	br	%r14
*/
float
extractsecondfloat (V4SF x)
{
  return x[1];
}

/*
** extractthirdfloat:
**	vrepf	%v0,%v24,2
**	br	%r14
*/
float
extractthirdfloat (V4SF x)
{
  return x[2];
}

/*
** extractfourthfloat:
**	vrepf	%v0,%v24,3
**	br	%r14
*/
float
extractfourthfloat (V4SF x)
{
  return x[3];
}

/*
** extractnthfloat:
**	vlgvf	(%r.),%v24,0\(%r2\)
**	vlvgf	%v0,\1,0
**	br	%r14
*/
float
extractnthfloat (V4SF x, int n)
{
  return x[n];
}

/*
** sumfirstfloat:
**	vfasb	%v0,%v24,%v26
**	br	%r14
*/
float
sumfirstfloat (V4SF x, V4SF y)
{
  return (x + y)[0];
}

/*
** extractfirst2:
**	vlr	%v0,%v24
**	br	%r14
*/
float
extractfirst2 (V2SF x)
{
  return x[0];
}

/*
** extractsecond2:
**	vrepf	%v0,%v24,1
**	br	%r14
*/
float
extractsecond2 (V2SF x)
{
  return x[1];
}

/*
** extractnth2:
**	vlgvf	(%r.),%v24,0\(%r2\)
**	vlvgf	%v0,\1,0
**	br	%r14
*/
float
extractnth2 (V2SF x, int n)
{
  return x[n];
}

/*
** extractsinglef:
**	vlr	%v0,%v24
**	br	%r14
*/
float
extractsinglef (V1SF x)
{
  return x[0];
}

/*
** extractsingled:
**	vlr	%v0,%v24
**	br	%r14
*/
double
extractsingled (V1DF x)
{
  return x[0];
}

/*
** extractsingleld:
**	vlr	(%v.),%v24
**	vst	\1,0\(%r2\),3
**	br	%r14
*/
long double
extractsingleld (V1TF x)
{
  return x[0];
}
