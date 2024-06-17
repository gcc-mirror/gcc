/* { dg-do compile { target lp64 } } */
/* { dg-options "-O2 -march=z13 -mzarch -fdump-tree-optimized" } */
/* { dg-final { check-function-bodies "**" "" "" } } */

/* As time of writing this, there is no support for 128-bit integer literals.
   Therefore, we have to emulate them as e.g. via two long literals.  However,
   this test is all about __int128 constants.  Thus, ensure that we end up with
   128-bit constants before expanding.  */
/* { dg-final { scan-tree-dump "= 0xe0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e;" "optimized" } } */
/* { dg-final { scan-tree-dump "= 0x7ffe7ffe7ffe7ffe7ffe7ffe7ffe7ffe;" "optimized" } } */
/* { dg-final { scan-tree-dump "= 0x7ffffffe7ffffffe7ffffffe7ffffffe;" "optimized" } } */
/* { dg-final { scan-tree-dump "= 0x7ffffffffffffffe7ffffffffffffffe;" "optimized" } } */

/*
** test_int128_via_vgmb:
**     vgmb	(%v[0-9]+),4,6
**     vst	\1,0\(%r2\),3
**     br	%r14
*/

__int128
test_int128_via_vgmb (void)
{
  return ((__int128) 0x0e0e0e0e0e0e0e0e << 64) | 0x0e0e0e0e0e0e0e0e;
}

/*
** test_int128_via_vgmh:
**     vgmh	(%v[0-9]+),1,14
**     vst	\1,0\(%r2\),3
**     br	%r14
*/

__int128
test_int128_via_vgmh (void)
{
  return ((__int128) 0x7ffe7ffe7ffe7ffe << 64) | 0x7ffe7ffe7ffe7ffe;
}

/*
** test_int128_via_vgmf:
**     vgmf	(%v[0-9]+),1,30
**     vst	\1,0\(%r2\),3
**     br	%r14
*/

__int128
test_int128_via_vgmf (void)
{
  return ((__int128) 0x7ffffffe7ffffffe << 64) | 0x7ffffffe7ffffffe;
}

/*
** test_int128_via_vgmg:
**     vgmg	(%v[0-9]+),1,62
**     vst	\1,0\(%r2\),3
**     br	%r14
*/

__int128
test_int128_via_vgmg (void)
{
  return ((__int128) 0x7ffffffffffffffe << 64) | 0x7ffffffffffffffe;
}
