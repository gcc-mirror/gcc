/* { dg-do compile } */
/* { dg-require-effective-target s390_vx } */
/* { dg-options "-O2 -march=z13 -mzarch" } */
/* { dg-final { check-function-bodies "**" "" "" } } */

/* Test loading constants which cannot be loaded via VECTOR GENERATE BYTE MASK
   nor via VECTOR REPLICATE IMMEDIATE but via VECTOR GENERATE MASK.  */

typedef short  v1hi __attribute__ ((vector_size (2)));
typedef short  v2hi __attribute__ ((vector_size (4)));
typedef short  v4hi __attribute__ ((vector_size (8)));
typedef short  v8hi __attribute__ ((vector_size (16)));

/*
** test_v1hi_via_vgmb:
**     vgm	%v24,0,2,0
**     br	%r14
*/

v1hi
test_v1hi_via_vgmb (void)
{
  return (v1hi){0xe0e0};
}

/*
** test_v2hi_via_vgmb:
**     vgm	%v24,0,2,0
**     br	%r14
*/

v2hi
test_v2hi_via_vgmb (void)
{
  return (v2hi){0xe0e0, 0xe0e0};
}

/*
** test_v4hi_via_vgmb:
**     vgm	%v24,0,2,0
**     br	%r14
*/

v4hi
test_v4hi_via_vgmb (void)
{
  return (v4hi){0xe0e0, 0xe0e0, 0xe0e0, 0xe0e0};
}

/*
** test_v8hi_via_vgmb:
**     vgm	%v24,0,2,0
**     br	%r14
*/

v8hi
test_v8hi_via_vgmb (void)
{
  return (v8hi){0xe0e0, 0xe0e0, 0xe0e0, 0xe0e0, 0xe0e0, 0xe0e0, 0xe0e0, 0xe0e0};
}

/*
** test_v1hi_via_vgmb_wrap:
**     vgm	%v24,5,2,0
**     br	%r14
*/

v1hi
test_v1hi_via_vgmb_wrap (void)
{
  return (v1hi){0xe7e7};
}

/*
** test_v2hi_via_vgmb_wrap:
**     vgm	%v24,5,2,0
**     br	%r14
*/

v2hi
test_v2hi_via_vgmb_wrap (void)
{
  return (v2hi){0xe7e7, 0xe7e7};
}

/*
** test_v4hi_via_vgmb_wrap:
**     vgm	%v24,5,2,0
**     br	%r14
*/

v4hi
test_v4hi_via_vgmb_wrap (void)
{
  return (v4hi){0xe7e7, 0xe7e7, 0xe7e7, 0xe7e7};
}

/*
** test_v8hi_via_vgmb_wrap:
**     vgm	%v24,5,2,0
**     br	%r14
*/

v8hi
test_v8hi_via_vgmb_wrap (void)
{
  return (v8hi){0xe7e7, 0xe7e7, 0xe7e7, 0xe7e7, 0xe7e7, 0xe7e7, 0xe7e7, 0xe7e7};
}

/*
** test_v1hi_via_vgmh:
**     vgm	%v24,5,10,1
**     br	%r14
*/

v1hi
test_v1hi_via_vgmh (void)
{
  return (v1hi){0x7e0};
}

/*
** test_v2hi_via_vgmh:
**     vgm	%v24,5,10,1
**     br	%r14
*/

v2hi
test_v2hi_via_vgmh (void)
{
  return (v2hi){0x7e0, 0x7e0};
}

/*
** test_v4hi_via_vgmh:
**     vgm	%v24,5,10,1
**     br	%r14
*/

v4hi
test_v4hi_via_vgmh (void)
{
  return (v4hi){0x7e0, 0x7e0, 0x7e0, 0x7e0};
}

/*
** test_v8hi_via_vgmh:
**     vgm	%v24,5,10,1
**     br	%r14
*/

v8hi
test_v8hi_via_vgmh (void)
{
  return (v8hi){0x7e0, 0x7e0, 0x7e0, 0x7e0, 0x7e0, 0x7e0, 0x7e0, 0x7e0};
}

/*
** test_v2hi_via_vgmf:
**     vgm	%v24,1,30,2
**     br	%r14
*/

v2hi
test_v2hi_via_vgmf (void)
{
  return (v2hi){0x7fff, 0xfffe};
}

/*
** test_v4hi_via_vgmf:
**     vgm	%v24,1,30,2
**     br	%r14
*/

v4hi
test_v4hi_via_vgmf (void)
{
  return (v4hi){0x7fff, 0xfffe, 0x7fff, 0xfffe};
}

/*
** test_v8hi_via_vgmf:
**     vgm	%v24,1,30,2
**     br	%r14
*/

v8hi
test_v8hi_via_vgmf (void)
{
  return (v8hi){0x7fff, 0xfffe, 0x7fff, 0xfffe, 0x7fff, 0xfffe, 0x7fff, 0xfffe};
}

/*
** test_v4hi_via_vgmg:
**     vgm	%v24,1,62,3
**     br	%r14
*/

v4hi
test_v4hi_via_vgmg (void)
{
  return (v4hi){0x7fff, 0xffff, 0xffff, 0xfffe};
}

/*
** test_v8hi_via_vgmg:
**     vgm	%v24,1,62,3
**     br	%r14
*/

v8hi
test_v8hi_via_vgmg (void)
{
  return (v8hi){0x7fff, 0xffff, 0xffff, 0xfffe, 0x7fff, 0xffff, 0xffff, 0xfffe};
}
