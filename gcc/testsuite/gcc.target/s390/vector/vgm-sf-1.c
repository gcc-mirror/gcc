/* { dg-do compile } */
/* { dg-require-effective-target s390_vxe } */
/* { dg-options "-O2 -march=z14 -mzarch" } */
/* { dg-final { check-function-bodies "**" "" "" } } */

/* Test loading constants which cannot be loaded via VECTOR GENERATE BYTE MASK
   nor via VECTOR REPLICATE IMMEDIATE but via VECTOR GENERATE MASK.  */

typedef float v1sf __attribute__ ((vector_size (4)));
typedef float v2sf __attribute__ ((vector_size (8)));
typedef float v4sf __attribute__ ((vector_size (16)));

/*
** test_v1sf_via_vgmb:
**     vgm	%v24,0,3,0
**     br	%r14
*/

v1sf
test_v1sf_via_vgmb (void)
{
  return (v1sf){-5.9654142e29};
}

/*
** test_v2sf_via_vgmb:
**     vgm	%v24,0,3,0
**     br	%r14
*/

v2sf
test_v2sf_via_vgmb (void)
{
  return (v2sf){-5.9654142e29, -5.9654142e29};
}

/*
** test_v4sf_via_vgmb:
**     vgm	%v24,0,3,0
**     br	%r14
*/

v4sf
test_v4sf_via_vgmb (void)
{
  return (v4sf){-5.9654142e29, -5.9654142e29, -5.9654142e29, -5.9654142e29};
}
