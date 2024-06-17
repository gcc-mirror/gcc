/* { dg-do compile } */
/* { dg-options "-O2 -march=z13 -mzarch" } */
/* { dg-final { check-function-bodies "**" "" "" } } */

typedef double v1df __attribute__ ((vector_size (8)));
typedef double v2df __attribute__ ((vector_size (16)));

/*
** test_v1df_via_vgmb:
**     vgmb	%v24,0,1
**     br	%r14
*/

v1df
test_v1df_via_vgmb (void)
{
  return (v1df){-8577.505882352939806878566741943359375};
}

/*
** test_v2df_via_vgmb:
**     vgmb	%v24,0,1
**     br	%r14
*/

v2df
test_v2df_via_vgmb (void)
{
  return (v2df){-8577.505882352939806878566741943359375, -8577.505882352939806878566741943359375};
}
