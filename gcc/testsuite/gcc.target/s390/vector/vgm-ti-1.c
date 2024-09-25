/* { dg-do compile } */
/* { dg-require-effective-target s390_vx } */
/* { dg-require-effective-target int128 } */
/* { dg-options "-O2 -march=z13 -mzarch -fdump-tree-optimized" } */
/* { dg-final { check-function-bodies "**" "" "" } } */

/* Test loading constants which cannot be loaded via VECTOR GENERATE BYTE MASK
   nor via VECTOR REPLICATE IMMEDIATE but via VECTOR GENERATE MASK.  */

/* As time of writing this, there is no support for 128-bit integer literals.
   Therefore, we have to emulate them as e.g. via two long literals.  However,
   this test is all about V1TI const vectors.  Thus, ensure that we end up with
   a V1TI const vector before expanding.  */
/* { dg-final { scan-tree-dump "{ 0xe0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e }" "optimized" } } */
/* { dg-final { scan-tree-dump "{ 0x7ffe7ffe7ffe7ffe7ffe7ffe7ffe7ffe }" "optimized" } } */
/* { dg-final { scan-tree-dump "{ 0x7ffffffe7ffffffe7ffffffe7ffffffe }" "optimized" } } */
/* { dg-final { scan-tree-dump "{ 0x7ffffffffffffffe7ffffffffffffffe }" "optimized" } } */

typedef __int128  v1ti __attribute__ ((vector_size (16)));
typedef long v2di __attribute__ ((vector_size (16)));

/*
** test_v1ti_via_vgmb:
**     vgm	%v24,4,6,0
**     br	%r14
*/

v1ti
test_v1ti_via_vgmb (void)
{
  return (v1ti)(v2di){0x0e0e0e0e0e0e0e0e, 0x0e0e0e0e0e0e0e0e};
}

/*
** test_v1ti_via_vgmh:
**     vgm	%v24,1,14,1
**     br	%r14
*/

v1ti
test_v1ti_via_vgmh (void)
{
  return (v1ti)(v2di){0x7ffe7ffe7ffe7ffe, 0x7ffe7ffe7ffe7ffe};
}

/*
** test_v1ti_via_vgmf:
**     vgm	%v24,1,30,2
**     br	%r14
*/

v1ti
test_v1ti_via_vgmf (void)
{
  return (v1ti)(v2di){0x7ffffffe7ffffffe, 0x7ffffffe7ffffffe};
}

/*
** test_v1ti_via_vgmg:
**     vgm	%v24,1,62,3
**     br	%r14
*/

v1ti
test_v1ti_via_vgmg (void)
{
  return (v1ti)(v2di){0x7ffffffffffffffe, 0x7ffffffffffffffe};
}
