/* { dg-do compile } */
/* { dg-options "-O2 -march=arch11 -mzarch" } */
/* { dg-final { check-function-bodies "**" "" } } */

typedef double V2DF __attribute__((vector_size(16)));
typedef float V4SF __attribute__((vector_size(16)));
typedef float V2SF __attribute__((vector_size(8)));
typedef double V1DF __attribute__((vector_size(8)));
typedef float V1SF __attribute__((vector_size(4)));
typedef long double V1TF __attribute__((vector_size(16)));

/*
** extractfirstdouble:
**	vsteg	%v24,0\(%r2\),0
**	br	%r14
*/
void
extractfirstdouble (double *res, V2DF x)
{
  *res = x[0];
}

/*
** extractseconddouble:
**	vsteg	%v24,0\(%r2\),1
**	br	%r14
*/
void
extractseconddouble (double *res, V2DF x)
{
  *res = x[1];
}

/*
** extractnthdouble:
**	vlgvg	(%r.),%v24,0\(%r3\)
**	stg	\1,0\(%r2\)
**	br	%r14
*/
void
extractnthdouble (double *res, V2DF x, int n)
{
  *res = x[n];
}

/*
** extractfirstfloat:
**	vstef	%v24,0\(%r2\),0
**	br	%r14
*/
void
extractfirstfloat (float *res, V4SF x)
{
  *res = x[0];
}

/*
** extractsecondfloat:
**	vstef	%v24,0\(%r2\),1
**	br	%r14
*/
void
extractsecondfloat (float *res, V4SF x)
{
  *res = x[1];
}

/*
** extractthirdfloat:
**	vstef	%v24,0\(%r2\),2
**	br	%r14
*/
void
extractthirdfloat (float *res, V4SF x)
{
  *res = x[2];
}

/*
** extractfourthfloat:
**	vstef	%v24,0\(%r2\),3
**	br	%r14
*/
void
extractfourthfloat (float *res, V4SF x)
{
  *res = x[3];
}

/*
** extractnthfloat:
**	vlgvf	(%r.),%v24,0\(%r3\)
**	st	\1,0\(%r2\)
**	br	%r14
*/
void
extractnthfloat (float *res, V4SF x, int n)
{
  *res = x[n];
}

/*
** extractfirst2:
**	vstef	%v24,0\(%r2\),0
**	br	%r14
*/
void
extractfirst2 (float *res, V2SF x)
{
  *res = x[0];
}

/*
** extractsecond2:
**	vstef	%v24,0\(%r2\),1
**	br	%r14
*/
void
extractsecond2 (float *res, V2SF x)
{
  *res = x[1];
}

/*
** extractnth2:
**	vlgvf	(%r.),%v24,0\(%r3\)
**	st	\1,0\(%r2\)
**	br	%r14
*/
void
extractnth2 (float *res, V2SF x, int n)
{
  *res = x[n];
}

/*
** extractsinglef:
**	vlr	%v(.),%v24
**	ste	%f\1,0\(%r2\)
**	br	%r14
*/
void
extractsinglef (float *res, V1SF x)
{
  *res = x[0];
}

/*
** extractsingled:
**	vsteg	%v24,0\(%r2\),0
**	br	%r14
*/
void
extractsingled (double *res, V1DF x)
{
  *res = x[0];
}

/*
** extractsingleld:
**	vst	%v24,0\(%r2\),3
**	br	%r14
*/
void
extractsingleld (long double *res, V1TF x)
{
  *res = x[0];
}
