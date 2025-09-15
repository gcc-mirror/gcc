/* Check fmin/fmax expanders for scalars on VXE targets.  */

/* { dg-do compile } */
/* { dg-options "-O2 -march=z14 -mzarch -fno-trapping-math" } */
/* { dg-final { check-function-bodies "**" "" } } */

/*
** dofmaxl:
**	vl	(%v.),0\(%r3\),3
**	vl	(%v.),0\(%r4\),3
**	wfmaxxb	(%v.),\1,\2,4
**	vst	\3,0\(%r2\),3
**	br	%r14
*/
long double
dofmaxl (long double d1, long double d2)
{
  return __builtin_fmaxl (d1, d2);
}

/*
** dofminl:
**	vl	(%v.),0\(%r3\),3
**	vl	(%v.),0\(%r4\),3
**	wfminxb	(%v.),\1,\2,4
**	vst	\3,0\(%r2\),3
**	br	%r14
*/
long double
dofminl (long double d1, long double d2)
{
  return __builtin_fminl (d1, d2);
}

/*
** dofmax:
**	wfmaxdb	%v0,%v0,%v2,4
**	br	%r14
*/
double
dofmax (double d1, double d2)
{
  return __builtin_fmax (d1, d2);
}

/*
** dofmin:
**	wfmindb	%v0,%v0,%v2,4
**	br	%r14
*/
double
dofmin (double d1, double d2)
{
  return __builtin_fmin (d1, d2);
}

/*
** dofmaxf:
**	wfmaxsb	%v0,%v0,%v2,4
**	br	%r14
*/
float
dofmaxf (float f1, float f2)
{
  return __builtin_fmaxf (f1, f2);
}

/*
** dofminf:
**	wfminsb	%v0,%v0,%v2,4
**	br	%r14
*/
float
dofminf (float f1, float f2)
{
  return __builtin_fminf (f1, f2);
}
