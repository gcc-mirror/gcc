/* { dg-do compile } */
/* { dg-require-effective-target s390_vx } */
/* { dg-options "-O2 -march=z13 -mzarch" } */
/* { dg-final { check-function-bodies "**" "" "" } } */

/* Test loading constants which cannot be loaded via VECTOR GENERATE BYTE MASK
   nor via VECTOR REPLICATE IMMEDIATE but via VECTOR GENERATE MASK.  */

typedef double v1df __attribute__ ((vector_size (8)));
typedef double v2df __attribute__ ((vector_size (16)));

/*
** test_v1df_via_vgmb:
**     vgm	%v24,0,1,0
**     br	%r14
*/

v1df
test_v1df_via_vgmb (void)
{
  return (v1df){-8577.505882352939806878566741943359375};
}

/*
** test_v2df_via_vgmb:
**     vgm	%v24,0,1,0
**     br	%r14
*/

v2df
test_v2df_via_vgmb (void)
{
  return (v2df){-8577.505882352939806878566741943359375, -8577.505882352939806878566741943359375};
}
