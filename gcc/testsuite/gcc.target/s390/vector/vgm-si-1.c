/* { dg-do compile } */
/* { dg-options "-O2 -march=z13 -mzarch" } */
/* { dg-final { check-function-bodies "**" "" "" } } */

typedef int  v1si __attribute__ ((vector_size (4)));
typedef int  v2si __attribute__ ((vector_size (8)));
typedef int  v4si __attribute__ ((vector_size (16)));

/*
** test_v1si_via_vgmb:
**     vgmb	%v24,0,2
**     br	%r14
*/

v1si
test_v1si_via_vgmb (void)
{
  return (v1si){0xe0e0e0e0};
}

/*
** test_v2si_via_vgmb:
**     vgmb	%v24,0,2
**     br	%r14
*/

v2si
test_v2si_via_vgmb (void)
{
  return (v2si){0xe0e0e0e0, 0xe0e0e0e0};
}

/*
** test_v4si_via_vgmb:
**     vgmb	%v24,0,2
**     br	%r14
*/

v4si
test_v4si_via_vgmb (void)
{
  return (v4si){0xe0e0e0e0, 0xe0e0e0e0, 0xe0e0e0e0, 0xe0e0e0e0};
}

/*
** test_v1si_via_vgmb_wrap:
**     vgmb	%v24,5,2
**     br	%r14
*/

v1si
test_v1si_via_vgmb_wrap (void)
{
  return (v1si){0xe7e7e7e7};
}

/*
** test_v2si_via_vgmb_wrap:
**     vgmb	%v24,5,2
**     br	%r14
*/

v2si
test_v2si_via_vgmb_wrap (void)
{
  return (v2si){0xe7e7e7e7, 0xe7e7e7e7};
}

/*
** test_v4si_via_vgmb_wrap:
**     vgmb	%v24,5,2
**     br	%r14
*/

v4si
test_v4si_via_vgmb_wrap (void)
{
  return (v4si){0xe7e7e7e7, 0xe7e7e7e7, 0xe7e7e7e7, 0xe7e7e7e7};
}

/*
** test_v1si_via_vgmh:
**     vgmh	%v24,5,10
**     br	%r14
*/

v1si
test_v1si_via_vgmh (void)
{
  return (v1si){0x7e007e0};
}

/*
** test_v2si_via_vgmh:
**     vgmh	%v24,5,10
**     br	%r14
*/

__attribute__ ((noipa))
v2si
test_v2si_via_vgmh (void)
{
  return (v2si){0x7e007e0, 0x7e007e0};
}

/*
** test_v4si_via_vgmh:
**     vgmh	%v24,5,10
**     br	%r14
*/

v4si
test_v4si_via_vgmh (void)
{
  return (v4si){0x7e007e0, 0x7e007e0, 0x7e007e0, 0x7e007e0};
}

/*
** test_v2si_via_vgmg:
**     vgmg	%v24,17,46
**     br	%r14
*/

__attribute__ ((noipa))
v2si
test_v2si_via_vgmg (void)
{
  return (v2si){0x7fff, 0xfffe0000};
}

/*
** test_v4si_via_vgmg:
**     vgmg	%v24,17,46
**     br	%r14
*/

v4si
test_v4si_via_vgmg (void)
{
  return (v4si){0x7fff, 0xfffe0000, 0x7fff, 0xfffe0000};
}

int main (void)
{
  test_v2si_via_vgmh ();
}
