/* { dg-do compile } */
/* { dg-options "-O2 -mlittle-endian" } */
/* { dg-final { check-function-bodies "**" "" "" } } */

#pragma GCC target ("arch=armv8-a")

typedef short v4hi __attribute__ ((vector_size (8)));
typedef char v8qi __attribute__ ((vector_size (8)));
typedef int v4si __attribute__ ((vector_size (16)));
typedef float v4sf __attribute__ ((vector_size (16)));
typedef short v8hi __attribute__ ((vector_size (16)));
typedef char v16qi __attribute__ ((vector_size (16)));

/*
** f_v4hi:
**	fmov	s0, s0
**	ret
*/
v4hi
f_v4hi (v4hi x)
{
  return __builtin_shuffle (x, (v4hi){ 0, 0, 0, 0 }, (v4hi){ 0, 1, 4, 5 });
}

/*
** g_v4hi:
**	(?:(?!fmov).)*
**	ret
*/
v4hi
g_v4hi (v4hi x)
{
  return __builtin_shuffle (x, (v4hi){ 0, 0, 0, 0 }, (v4hi){ 3, 1, 4, 2 });
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
			    (v8hi){ 0, 1, 8, 9, 10, 11, 12, 13 });
}

/*
** f_v4si:
**	fmov	d0, d0
**	ret
*/
v4si
f_v4si (v4si x)
{
  return __builtin_shuffle (x, (v4si){ 0, 0, 0, 0 }, (v4si){ 0, 1, 4, 5 });
}

/*
** g_v4si:
**	fmov	d0, d0
**	ret
*/
v4si
g_v4si (v4si x)
{
  return __builtin_shuffle ((v4si){ 0, 0, 0, 0 }, x, (v4si){ 4, 5, 2, 3 });
}

/*
** h_v4si:
**	fmov	s0, s0
**	ret
*/
v4si
h_v4si (v4si x)
{
  return __builtin_shuffle (x, (v4si){ 0, 0, 0, 0 }, (v4si){ 0, 4, 5, 6 });
}

/*
** f_v4sf:
**	fmov	d0, d0
**	ret
*/
v4sf
f_v4sf (v4sf x)
{
  return __builtin_shuffle (x, (v4sf){ 0, 0, 0, 0 }, (v4si){ 0, 1, 6, 7 });
}

/*
** f_v8qi:
**	fmov	s0, s0
**	ret
*/
v8qi
f_v8qi (v8qi x)
{
  return __builtin_shuffle (x, (v8qi){ 0, 0, 0, 0, 0, 0, 0, 0 },
			    (v8qi){ 0, 1, 2, 3, 10, 11, 12, 13 });
}

/*
** f_v16qi:
**	fmov	d0, d0
**	ret
*/
v16qi
f_v16qi (v16qi x)
{
  return __builtin_shuffle (
      x, (v16qi){ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 },
      (v16qi){ 0, 1, 2, 3, 4, 5, 6, 7, 16, 17, 18, 19, 20, 21, 22, 23 });
}

/*
** g_v16qi:
**	fmov	s0, s0
**	ret
*/
v16qi
g_v16qi (v16qi x)
{
  return __builtin_shuffle (
      x, (v16qi){ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 },
      (v16qi){ 0, 1, 2, 3, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27 });
}

