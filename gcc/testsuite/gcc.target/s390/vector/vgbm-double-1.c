/* { dg-do compile } */
/* { dg-require-effective-target s390_vx } */
/* { dg-options "-O2 -march=z13 -mzarch" } */
/* { dg-final { check-function-bodies "**" "" "" } } */

/* Test loading constants which cannot be loaded via VECTOR GENERATE MASK nor
   via VECTOR REPLICATE IMMEDIATE but via VECTOR GENERATE BYTE MASK.  */

typedef double v1df __attribute__ ((vector_size (8)));
typedef double v2df __attribute__ ((vector_size (16)));

/*
** test_v1df:
**     vgbm	%v24,20480
**     br	%r14
*/

v1df
test_v1df (void)
{
  return (v1df){7.064161009310759e-304};
}

/*
** test_v2df:
**     vgbm	%v24,20560
**     br	%r14
*/

v2df
test_v2df (void)
{
  return (v2df){7.064161009310759e-304, 7.064161009310759e-304};
}
