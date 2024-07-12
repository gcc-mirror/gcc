/* { dg-do compile } */
/* { dg-require-effective-target s390_vx } */
/* { dg-require-effective-target int128 } */
/* { dg-options "-O2 -march=z13 -mzarch -fdump-tree-optimized" } */
/* { dg-final { check-function-bodies "**" "" "" } } */

/* Test loading constants which cannot be loaded via VECTOR GENERATE BYTE MASK
   nor via VECTOR GENERATE MASK but via VECTOR REPLICATE IMMEDIATE.  */

/* As time of writing this, there is no support for 128-bit integer literals.
   Therefore, we have to emulate them as e.g. via two long literals.  However,
   this test is all about V1TI const vectors.  Thus, ensure that we end up with
   a V1TI const vector before expanding.  */
/* { dg-final { scan-tree-dump "{ -0x55555555555555555555555555555556 }" "optimized" } } */
/* { dg-final { scan-tree-dump "{ -0x41024102410241024102410241024103 }" "optimized" } } */
/* { dg-final { scan-tree-dump "{ -0x4102000041020000410200004103 }" "optimized" } } */
/* { dg-final { scan-tree-dump "{ -0x41020000000000004103 }" "optimized" } } */

typedef __int128 v1ti __attribute__ ((vector_size (16)));
typedef long long v2di __attribute__ ((vector_size (16)));

/*
** test_v1ti_via_vrepib:
**     vrepi	%v24,-86,0
**     br	%r14
*/

v1ti
test_v1ti_via_vrepib (void)
{
  return (v1ti)(v2di){0xaaaaaaaaaaaaaaaa, 0xaaaaaaaaaaaaaaaa};
}

/*
** test_v1ti_via_vrepih:
**     vrepi	%v24,-16643,1
**     br	%r14
*/

v1ti
test_v1ti_via_vrepih (void)
{
  return (v1ti)(v2di){0xbefdbefdbefdbefd, 0xbefdbefdbefdbefd};
}

/*
** test_v1ti_via_vrepif:
**     vrepi	%v24,-16643,2
**     br	%r14
*/

v1ti
test_v1ti_via_vrepif (void)
{
  return (v1ti)(v2di){0xffffbefdffffbefd, 0xffffbefdffffbefd};
}

/*
** test_v1ti_via_vrepig:
**     vrepi	%v24,-16643,3
**     br	%r14
*/

v1ti
test_v1ti_via_vrepig (void)
{
  return (v1ti)(v2di){-16643, -16643};
}
