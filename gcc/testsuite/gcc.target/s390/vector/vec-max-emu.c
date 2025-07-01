/* { dg-do run } */
/* { dg-options "-O3 -mzarch -march=z13 -save-temps -fno-stack-protector" } */
/* { dg-require-effective-target int128 } */
/* { dg-final { check-function-bodies "**" "" "" } } */
/* { dg-final { scan-assembler-not {\tvmxq\t} } } */

#include <assert.h>

typedef __attribute__ ((vector_size (16))) signed long long v2di;
typedef __attribute__ ((vector_size (16))) signed __int128 v1ti;

/*
** my_max:
**	vchlg	%v[0-9]+,%v[0-9]+,%v[0-9]+
**	vceqg	%v[0-9]+,%v[0-9]+,%v[0-9]+
**	vpdi	%v[0-9]+,%v[0-9]+,%v[0-9]+,4
**	vchg	%v[0-9]+,%v[0-9]+,%v[0-9]+
**	vn	%v[0-9]+,%v[0-9]+,%v[0-9]+
**	vo	%v[0-9]+,%v[0-9]+,%v[0-9]+
**	vrepg	%v[0-9]+,%v[0-9]+,0
**	vsel	%v[0-9]+,%v[0-9]+,%v[0-9]+,%v[0-9]+
**	br	%r14
*/
__attribute__ ((noipa)) v1ti
my_max (v1ti x, v1ti y)
{
  return __builtin_s390_vec_max (x, y);
}

int
main (void)
{
  v2di x, y, z;

  x = (v2di){ -1, -42 };
  y = (v2di){  0,  42 };
  z = (v2di) my_max ((v1ti) x, (v1ti) y);
  assert (z[0] == 0 && z[1] == 42);
  z = (v2di) my_max ((v1ti) y, (v1ti) x);
  assert (z[0] == 0 && z[1] == 42);

  x = (v2di){ 42, 42 };
  y = (v2di){ 42, 43 };
  z = (v2di) my_max ((v1ti) x, (v1ti) y);
  assert (z[0] == 42 && z[1] == 43);
  z = (v2di) my_max ((v1ti) y, (v1ti) x);
  assert (z[0] == 42 && z[1] == 43);

  x = (v2di){ 42, 42 };
  y = (v2di){ 43, 42 };
  z = (v2di) my_max ((v1ti) x, (v1ti) y);
  assert (z[0] == 43 && z[1] == 42);
  z = (v2di) my_max ((v1ti) y, (v1ti) x);
  assert (z[0] == 43 && z[1] == 42);

  return 0;
}
