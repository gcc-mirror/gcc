/* { dg-do compile } */
/* { dg-require-effective-target s390_vx } */
/* { dg-require-effective-target int128 } */
/* { dg-options "-O2 -march=z13 -mzarch -fdump-tree-optimized" } */
/* { dg-final { check-function-bodies "**" "" "" } } */

/* Test loading constants which cannot be loaded via VECTOR GENERATE BYTE MASK
   nor via VECTOR GENERATE MASK but via VECTOR REPLICATE IMMEDIATE.  */

/* As time of writing this, there is no support for 128-bit integer literals.
   Therefore, we have to emulate them as e.g. via two long literals.  However,
   this test is all about int128 const vectors.  Thus, ensure that we end up with
   a int128 const vector before expanding.  */
/* { dg-final { scan-tree-dump "= -0x55555555555555555555555555555556;" "optimized" } } */
/* { dg-final { scan-tree-dump "= -0x41024102410241024102410241024103;" "optimized" } } */
/* { dg-final { scan-tree-dump "= -0x4102000041020000410200004103;" "optimized" } } */
/* { dg-final { scan-tree-dump "= -0x41020000000000004103;" "optimized" } } */

/*
** test_int128_via_vrepib:
**     vrepi	(%v[0-9]+),-86,0
**     vst	\1,0\(%r2\),3
**     br	%r14
*/

__int128
test_int128_via_vrepib (void)
{
  return ((__int128) 0xaaaaaaaaaaaaaaaa << 64) | 0xaaaaaaaaaaaaaaaa;
}

/*
** test_int128_via_vrepih:
**     vrepi	(%v[0-9]+),-16643,1
**     vst	\1,0\(%r2\),3
**     br	%r14
*/

__int128
test_int128_via_vrepih (void)
{
  return ((__int128) 0xbefdbefdbefdbefd << 64) | 0xbefdbefdbefdbefd;
}

/*
** test_int128_via_vrepif:
**     vrepi	(%v[0-9]+),-16643,2
**     vst	\1,0\(%r2\),3
**     br	%r14
*/

__int128
test_int128_via_vrepif (void)
{
  return ((__int128) 0xffffbefdffffbefd << 64) | 0xffffbefdffffbefd;
}

/*
** test_int128_via_vrepig:
**     vrepi	(%v[0-9]+),-16643,3
**     vst	\1,0\(%r2\),3
**     br	%r14
*/

__int128
test_int128_via_vrepig (void)
{
  return ((__int128) 0xffffffffffffbefd << 64) | 0xffffffffffffbefd;
}
