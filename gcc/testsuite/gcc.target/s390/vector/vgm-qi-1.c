/* { dg-do compile } */
/* { dg-options "-O2 -march=z13 -mzarch" } */
/* { dg-final { check-function-bodies "**" "" "" } } */

typedef signed char  v1qi __attribute__ ((vector_size (1)));
typedef signed char  v2qi __attribute__ ((vector_size (2)));
typedef signed char  v4qi __attribute__ ((vector_size (4)));
typedef signed char  v8qi __attribute__ ((vector_size (8)));
typedef signed char  v16qi __attribute__ ((vector_size (16)));

/*
** test_v1qi_via_vgmb:
**     vgmb	%v24,0,2
**     br	%r14
*/

v1qi
test_v1qi_via_vgmb (void)
{
  return (v1qi){0xe0};
}

/*
** test_v2qi_via_vgmb:
**     vgmb	%v24,0,2
**     br	%r14
*/

v2qi
test_v2qi_via_vgmb (void)
{
  return (v2qi){0xe0, 0xe0};
}

/*
** test_v4qi_via_vgmb:
**     vgmb	%v24,0,2
**     br	%r14
*/

v4qi
test_v4qi_via_vgmb (void)
{
  return (v4qi){0xe0, 0xe0, 0xe0, 0xe0};
}

/*
** test_v8qi_via_vgmb:
**     vgmb	%v24,0,2
**     br	%r14
*/

v8qi
test_v8qi_via_vgmb (void)
{
  return (v8qi){0xe0, 0xe0, 0xe0, 0xe0, 0xe0, 0xe0, 0xe0, 0xe0};
}

/*
** test_v16qi_via_vgmb:
**     vgmb	%v24,0,2
**     br	%r14
*/

v16qi
test_v16qi_via_vgmb (void)
{
  return (v16qi){0xe0, 0xe0, 0xe0, 0xe0, 0xe0, 0xe0, 0xe0, 0xe0, 0xe0, 0xe0, 0xe0, 0xe0, 0xe0, 0xe0, 0xe0, 0xe0};
}

/*
** test_v1qi_via_vgmb_wrap:
**     vgmb	%v24,5,2
**     br	%r14
*/

v1qi
test_v1qi_via_vgmb_wrap (void)
{
  return (v1qi){0xe7};
}

/*
** test_v2qi_via_vgmb_wrap:
**     vgmb	%v24,5,2
**     br	%r14
*/

v2qi
test_v2qi_via_vgmb_wrap (void)
{
  return (v2qi){0xe7, 0xe7};
}

/*
** test_v4qi_via_vgmb_wrap:
**     vgmb	%v24,5,2
**     br	%r14
*/

v4qi
test_v4qi_via_vgmb_wrap (void)
{
  return (v4qi){0xe7, 0xe7, 0xe7, 0xe7};
}

/*
** test_v8qi_via_vgmb_wrap:
**     vgmb	%v24,5,2
**     br	%r14
*/

v8qi
test_v8qi_via_vgmb_wrap (void)
{
  return (v8qi){0xe7, 0xe7, 0xe7, 0xe7, 0xe7, 0xe7, 0xe7, 0xe7};
}

/*
** test_v16qi_via_vgmb_wrap:
**     vgmb	%v24,5,2
**     br	%r14
*/

v16qi
test_v16qi_via_vgmb_wrap (void)
{
  return (v16qi){0xe7, 0xe7, 0xe7, 0xe7, 0xe7, 0xe7, 0xe7, 0xe7, 0xe7, 0xe7, 0xe7, 0xe7, 0xe7, 0xe7, 0xe7, 0xe7};
}

/*
** test_v2qi_via_vgmh:
**     vgmh	%v24,1,14
**     br	%r14
*/

v2qi
test_v2qi_via_vgmh (void)
{
  return (v2qi){0x7f, 0xfe};
}

/*
** test_v4qi_via_vgmh:
**     vgmh	%v24,1,14
**     br	%r14
*/

v4qi
test_v4qi_via_vgmh (void)
{
  return (v4qi){0x7f, 0xfe, 0x7f, 0xfe};
}

/*
** test_v8qi_via_vgmh:
**     vgmh	%v24,1,14
**     br	%r14
*/

v8qi
test_v8qi_via_vgmh (void)
{
  return (v8qi){0x7f, 0xfe, 0x7f, 0xfe, 0x7f, 0xfe, 0x7f, 0xfe};
}

/*
** test_v16qi_via_vgmh:
**     vgmh	%v24,1,14
**     br	%r14
*/

v16qi
test_v16qi_via_vgmh (void)
{
  return (v16qi){0x7f, 0xfe, 0x7f, 0xfe, 0x7f, 0xfe, 0x7f, 0xfe, 0x7f, 0xfe, 0x7f, 0xfe, 0x7f, 0xfe, 0x7f, 0xfe};
}

/*
** test_v4qi_via_vgmf:
**     vgmf	%v24,1,30
**     br	%r14
*/

v4qi
test_v4qi_via_vgmf (void)
{
  return (v4qi){0x7f, 0xff, 0xff, 0xfe};
}

/*
** test_v8qi_via_vgmf:
**     vgmf	%v24,1,30
**     br	%r14
*/

v8qi
test_v8qi_via_vgmf (void)
{
  return (v8qi){0x7f, 0xff, 0xff, 0xfe, 0x7f, 0xff, 0xff, 0xfe};
}

/*
** test_v8qi_via_vgmg:
**     vgmg	%v24,1,62
**     br	%r14
*/

v8qi
test_v8qi_via_vgmg (void)
{
  return (v8qi){0x7f, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xfe};
}
