/* { dg-do compile } */
/* { dg-require-effective-target s390_vxe } */
/* { dg-options "-O2 -march=z14 -mzarch" } */
/* { dg-final { check-function-bodies "**" "" "" } } */

/* Test loading constants which cannot be loaded via VECTOR GENERATE MASK nor
   via VECTOR REPLICATE IMMEDIATE but via VECTOR GENERATE BYTE MASK.  */

typedef long double v1tf __attribute__ ((vector_size (16)));

/*
** test_v1tf:
**     vgbm	%v24,20560
**     br	%r14
*/

v1tf
test_v1tf (void)
{
  return (v1tf){9.77049323250296736880493184970933526e-4856L};
}

/*
** test_longdouble:
**     vgbm	(%v[0-9]+),20560
**     vst	\1,0\(%r2\),3
**     br	%r14
*/

long double
test_longdouble (void)
{
  return 9.77049323250296736880493184970933526e-4856L;
}
