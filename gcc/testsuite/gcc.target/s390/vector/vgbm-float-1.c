/* { dg-do compile } */
/* { dg-require-effective-target s390_vxe } */
/* { dg-options "-O2 -march=z14 -mzarch" } */
/* { dg-final { check-function-bodies "**" "" "" } } */

/* Test loading constants which cannot be loaded via VECTOR GENERATE MASK nor
   via VECTOR REPLICATE IMMEDIATE but via VECTOR GENERATE BYTE MASK.  */

typedef float v2sf __attribute__ ((vector_size (8)));
typedef float v4sf __attribute__ ((vector_size (16)));

/*
** test_v2sf:
**     vgbm	%v24,20480
**     br	%r14
*/

v2sf
test_v2sf (void)
{
  return (v2sf){2.34184089e-38f, 0.f};
}

/*
** test_v4sf:
**     vgbm	%v24,20560
**     br	%r14
*/

v4sf
test_v4sf (void)
{
  return (v4sf){2.34184089e-38f, 0.f, 2.34184089e-38f, 0.f};
}
