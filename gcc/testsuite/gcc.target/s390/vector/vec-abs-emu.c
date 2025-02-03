/* { dg-do run } */
/* { dg-options "-O3 -mzarch -march=z13 -save-temps" } */
/* { dg-require-effective-target int128 } */
/* { dg-final { check-function-bodies "**" "" "" } } */
/* { dg-final { scan-assembler-not {\tvlpq\t} } } */

#include <assert.h>

typedef __attribute__ ((vector_size (16))) signed long long v2di;
typedef __attribute__ ((vector_size (16))) signed __int128 v1ti;

/*
** my_abs:
**	vzero	%v[0-9]+
**	vchlg	%v[0-9]+,%v[0-9]+,%v[0-9]+
**	vceqg	%v[0-9]+,%v[0-9]+,%v[0-9]+
**	vpdi	%v[0-9]+,%v[0-9]+,%v[0-9]+,4
**	vchg	%v[0-9]+,%v[0-9]+,%v[0-9]+
**	vn	%v[0-9]+,%v[0-9]+,%v[0-9]+
**	vo	%v[0-9]+,%v[0-9]+,%v[0-9]+
**	vrepg	%v[0-9]+,%v[0-9]+,0
**	vsq	%v[0-9]+,%v[0-9]+,%v[0-9]+
**	vsel	%v[0-9]+,%v[0-9]+,%v[0-9]+,%v[0-9]+
**	br	%r14
*/
__attribute__ ((noipa)) v1ti
my_abs (v1ti x)
{
  return __builtin_s390_vec_abs (x);
}

int
main (void)
{
  v2di x;

  x = (v2di){ -1, -42 };
  x = (v2di) my_abs ((v1ti) x);
  assert (x[0] == 0 && x[1] == 42);

  x = (v2di){ 0, 42 };
  x = (v2di) my_abs ((v1ti) x);
  assert (x[0] == 0 && x[1] == 42);

  return 0;
}
