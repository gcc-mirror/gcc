/* { dg-do compile } */
/* { dg-require-effective-target s390_vx } */
/* { dg-options "-O2 -march=z13 -mzarch" } */
/* { dg-final { check-function-bodies "**" "" "" } } */

/* Test loading constants which cannot be loaded via VECTOR GENERATE BYTE MASK
   nor via VECTOR REPLICATE IMMEDIATE but via VECTOR GENERATE MASK.  */

typedef int  v1si __attribute__ ((vector_size (4)));
typedef int  v2si __attribute__ ((vector_size (8)));
typedef int  v4si __attribute__ ((vector_size (16)));

/*
** test_v1si_via_vgmb:
**     vgm	%v24,0,2,0
**     br	%r14
*/

v1si
test_v1si_via_vgmb (void)
{
  return (v1si){0xe0e0e0e0};
}

/*
** test_v2si_via_vgmb:
**     vgm	%v24,0,2,0
**     br	%r14
*/

v2si
test_v2si_via_vgmb (void)
{
  return (v2si){0xe0e0e0e0, 0xe0e0e0e0};
}

/*
** test_v4si_via_vgmb:
**     vgm	%v24,0,2,0
**     br	%r14
*/

v4si
test_v4si_via_vgmb (void)
{
  return (v4si){0xe0e0e0e0, 0xe0e0e0e0, 0xe0e0e0e0, 0xe0e0e0e0};
}

/*
** test_v1si_via_vgmb_wrap:
**     vgm	%v24,5,2,0
**     br	%r14
*/

v1si
test_v1si_via_vgmb_wrap (void)
{
  return (v1si){0xe7e7e7e7};
}

/*
** test_v2si_via_vgmb_wrap:
**     vgm	%v24,5,2,0
**     br	%r14
*/

v2si
test_v2si_via_vgmb_wrap (void)
{
  return (v2si){0xe7e7e7e7, 0xe7e7e7e7};
}

/*
** test_v4si_via_vgmb_wrap:
**     vgm	%v24,5,2,0
**     br	%r14
*/

v4si
test_v4si_via_vgmb_wrap (void)
{
  return (v4si){0xe7e7e7e7, 0xe7e7e7e7, 0xe7e7e7e7, 0xe7e7e7e7};
}

/*
** test_v1si_via_vgmh:
**     vgm	%v24,5,10,1
**     br	%r14
*/

v1si
test_v1si_via_vgmh (void)
{
  return (v1si){0x7e007e0};
}

/*
** test_v2si_via_vgmh:
**     vgm	%v24,5,10,1
**     br	%r14
*/

v2si
test_v2si_via_vgmh (void)
{
  return (v2si){0x7e007e0, 0x7e007e0};
}

/*
** test_v4si_via_vgmh:
**     vgm	%v24,5,10,1
**     br	%r14
*/

v4si
test_v4si_via_vgmh (void)
{
  return (v4si){0x7e007e0, 0x7e007e0, 0x7e007e0, 0x7e007e0};
}

/*
** test_v1si_via_vgmf:
**     vgm	%v24,1,30,2
**     br	%r14
*/

v1si
test_v1si_via_vgmf (void)
{
  return (v1si){0x7ffffffe};
}

/*
** test_v2si_via_vgmf:
**     vgm	%v24,1,30,2
**     br	%r14
*/

v2si
test_v2si_via_vgmf (void)
{
  return (v2si){0x7ffffffe, 0x7ffffffe};
}

/*
** test_v4si_via_vgmf:
**     vgm	%v24,1,30,2
**     br	%r14
*/

v4si
test_v4si_via_vgmf (void)
{
  return (v4si){0x7ffffffe, 0x7ffffffe, 0x7ffffffe, 0x7ffffffe};
}

/*
** test_v2si_via_vgmg:
**     vgm	%v24,17,46,3
**     br	%r14
*/

v2si
test_v2si_via_vgmg (void)
{
  return (v2si){0x7fff, 0xfffe0000};
}

/*
** test_v4si_via_vgmg:
**     vgm	%v24,17,46,3
**     br	%r14
*/

v4si
test_v4si_via_vgmg (void)
{
  return (v4si){0x7fff, 0xfffe0000, 0x7fff, 0xfffe0000};
}
