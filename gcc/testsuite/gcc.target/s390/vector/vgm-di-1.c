/* { dg-do compile } */
/* { dg-options "-O2 -march=z13 -mzarch" } */
/* { dg-final { check-function-bodies "**" "" "" } } */

typedef long long v1di __attribute__ ((vector_size (8)));
typedef long long v2di __attribute__ ((vector_size (16)));

/*
** test_v1di_via_vgmb:
**     vgmb	%v24,0,2
**     br	%r14
*/

v1di
test_v1di_via_vgmb (void)
{
  return (v1di){0xe0e0e0e0e0e0e0e0};
}

/*
** test_v2di_via_vgmb:
**     vgmb	%v24,0,2
**     br	%r14
*/

v2di
test_v2di_via_vgmb (void)
{
  return (v2di){0xe0e0e0e0e0e0e0e0, 0xe0e0e0e0e0e0e0e0};
}

/*
** test_v1di_via_vgmb_wrap:
**     vgmb	%v24,5,2
**     br	%r14
*/

v1di
test_v1di_via_vgmb_wrap (void)
{
  return (v1di){0xe7e7e7e7e7e7e7e7};
}

/*
** test_v2di_via_vgmb_wrap:
**     vgmb	%v24,5,2
**     br	%r14
*/

v2di
test_v2di_via_vgmb_wrap (void)
{
  return (v2di){0xe7e7e7e7e7e7e7e7, 0xe7e7e7e7e7e7e7e7};
}

/*
** test_v1di_via_vgmh:
**     vgmh	%v24,5,10
**     br	%r14
*/

v1di
test_v1di_via_vgmh (void)
{
  return (v1di){0x7e007e007e007e0};
}

/*
** test_v2di_via_vgmh:
**     vgmh	%v24,5,10
**     br	%r14
*/

v2di
test_v2di_via_vgmh (void)
{
  return (v2di){0x7e007e007e007e0, 0x7e007e007e007e0};
}

/*
** test_v1di_via_vgmg:
**     vgmg	%v24,17,46
**     br	%r14
*/

v1di
test_v1di_via_vgmg (void)
{
  return (v1di){0x7ffffffe0000};
}

/*
** test_v2di_via_vgmg:
**     vgmg	%v24,17,46
**     br	%r14
*/

v2di
test_v2di_via_vgmg (void)
{
  return (v2di){0x7ffffffe0000, 0x7ffffffe0000};
}
