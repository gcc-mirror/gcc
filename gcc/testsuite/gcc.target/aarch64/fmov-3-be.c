/* { dg-do compile } */
/* { dg-options "-O2 -mbig-endian" } */
/* { dg-final { check-function-bodies "**" "" "" } } */

#pragma GCC target ("arch=armv8-a")

typedef short v4hi __attribute__ ((vector_size (8)));
typedef int v4si __attribute__ ((vector_size (16)));
typedef float v4sf __attribute__ ((vector_size (16)));
typedef short v8hi __attribute__ ((vector_size (16)));

/*
** f_v4hi:
**	fmov	s0, s0
**	ret
*/
v4hi
f_v4hi (v4hi x)
{
  return __builtin_shuffle (x, (v4hi){ 0, 0, 0, 0 }, (v4hi){ 4, 5, 2, 3 });
}

/*
** f_v8hi:
**	fmov	s0, s0
**	ret
*/
v8hi
f_v8hi (v8hi x)
{
  return __builtin_shuffle (x, (v8hi){ 0, 0, 0, 0, 0, 0, 0, 0 },
			    (v8hi){ 8, 9, 10, 11, 12, 13, 6, 7 });
}

/*
** f_v4si:
**	fmov	d0, d0
**	ret
*/
v4si
f_v4si (v4si x)
{
  return __builtin_shuffle (x, (v4si){ 0, 0, 0, 0 }, (v4si){ 6, 7, 2, 3 });
}

/*
** g_v4si:
**	fmov	d0, d0
**	ret
*/
v4si
g_v4si (v4si x)
{
  return __builtin_shuffle ((v4si){ 0, 0, 0, 0 }, x, (v4si){ 2, 3, 6, 7 });
}

/*
** h_v4si:
**	fmov	s0, s0
**	ret
*/
v4si
h_v4si (v4si x)
{
  return __builtin_shuffle (x, (v4si){ 0, 0, 0, 0 }, (v4si){ 4, 5, 6, 3 });
}

/*
** f_v4sf:
**	fmov	d0, d0
**	ret
*/
v4sf
f_v4sf (v4sf x)
{
  return __builtin_shuffle (x, (v4sf){ 0, 0, 0, 0 }, (v4si){ 6, 7, 2, 3 });
}
