/* { dg-do compile { target lp64 } } */
/* { dg-require-effective-target s390_vx } */
/* { dg-require-effective-target int128 } */
/* { dg-options "-O2 -march=z13 -mzarch -fdump-tree-optimized" } */
/* { dg-final { check-function-bodies "**" "" "" } } */

/* Test loading constants which cannot be loaded via VECTOR GENERATE BYTE MASK
   nor via VECTOR REPLICATE IMMEDIATE but via VECTOR GENERATE MASK.  */

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
**     vgm	(%v[0-9]+),4,6,0
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
**     vgm	(%v[0-9]+),1,14,1
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
**     vgm	(%v[0-9]+),1,30,2
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
**     vgm	(%v[0-9]+),1,62,3
**     vst	\1,0\(%r2\),3
**     br	%r14
*/

__int128
test_int128_via_vgmg (void)
{
  return ((__int128) 0x7ffffffffffffffe << 64) | 0x7ffffffffffffffe;
}
