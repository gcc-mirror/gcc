/* { dg-do compile } */
/* { dg-require-effective-target s390_vx } */
/* { dg-require-effective-target int128 } */
/* { dg-options "-O2 -march=z13 -mzarch -fdump-tree-optimized" } */
/* { dg-final { check-function-bodies "**" "" "" } } */

/* Test loading constants which cannot be loaded via VECTOR GENERATE MASK nor
   via VECTOR REPLICATE IMMEDIATE but via VECTOR GENERATE BYTE MASK.  */

/* As time of writing this, there is no support for 128-bit integer literals.
   Therefore, we have to emulate them as e.g. via two long literals.  However,
   this test is all about V1TI const vectors.  Thus, ensure that we end up with
   a V1TI const vector before expanding.  Likewise, for 128-bit scalar.  */
/* { dg-final { scan-tree-dump "{ 0xff00ff0000000000ff00ff00000000 }" "optimized" } } */
/* { dg-final { scan-tree-dump "= 0xff00ff0000000000ff00ff00000000;" "optimized" } } */

typedef long long v2di __attribute__ ((vector_size (16)));
typedef __int128 v1ti __attribute__ ((vector_size (16)));

/*
** test_v1ti:
**     vgbm	%v24,20560
**     br	%r14
*/

v1ti
test_v1ti (void)
{
  return (v1ti)(v2di){0xff00ff00000000, 0xff00ff00000000};
}

/*
** test_int128:
**     vgbm	(%v[0-9]+),20560
**     vst	\1,0\(%r2\),3
**     br	%r14
*/

__int128
test_int128 (void)
{
  return ((__int128) 0xff00ff00000000 << 64) | 0xff00ff00000000;
}
