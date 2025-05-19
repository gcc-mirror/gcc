/* { dg-do compile } */
/* { dg-options "-O2 -mbig-endian" } */
/* { dg-final { check-function-bodies "**" "" "" } } */

typedef short v4hi __attribute__ ((vector_size (8)));
typedef char v8qi __attribute__ ((vector_size (8)));
typedef int v4si __attribute__ ((vector_size (16)));
typedef float v4sf __attribute__ ((vector_size (16)));
typedef short v8hi __attribute__ ((vector_size (16)));
typedef char v16qi __attribute__ ((vector_size (16)));


/*
** f_v4hi:
**	movi	v([0-9]+).2s, 0xff, msl 8
**	and	v0.8b, (?:v0.8b, v\1.8b|v\1.8b, v0.8b)
**	ret
*/
v4hi
f_v4hi (v4hi x)
{
  return __builtin_shuffle (x, (v4hi){ 0, 0, 0, 0 }, (v4hi){ 4, 1, 6, 3 });
}

/*
** g_v4hi:
**	mvni	v([0-9]+).2s, 0xff, msl 8
**	and	v0.8b, (?:v0.8b, v\1.8b|v\1.8b, v0.8b)
**	ret
*/
v4hi
g_v4hi (v4hi x)
{
  return __builtin_shuffle (x, (v4hi){ 0, 0, 0, 0 }, (v4hi){ 0, 5, 2, 7 });
}

/*
** f_v8hi:
**	...
**	and	v0.16b, (?:v0.16b, v[0-9]+.16b|v[0-9]+.16b, v0.16b)
**	ret
*/
v8hi
f_v8hi (v8hi x)
{
  return __builtin_shuffle (x, (v8hi){ 0, 0, 0, 0, 0, 0, 0, 0 },
			    (v8hi){ 0, 8, 2, 9, 4, 10, 12, 11 });
}

/*
** f_v4si:
**	movi	v([0-9]+).2d, 0xffffffff00000000
**	and	v0.16b, (?:v0.16b, v\1.16b|v\1.16b, v0.16b)
**	ret
*/
v4si
f_v4si (v4si x)
{
  return __builtin_shuffle (x, (v4si){ 0, 0, 0, 0 }, (v4si){ 0, 4, 2, 5 });
}

/*
** g_v4si:
**	movi	v([0-9]+).2d, 0xffffffff
**	and	v0.16b, (?:v0.16b, v\1.16b|v\1.16b, v0.16b)
**	ret
*/
v4si
g_v4si (v4si x)
{
  return __builtin_shuffle ((v4si){ 0, 0, 0, 0 }, x, (v4si){ 1, 5, 3, 7 });
}

/*
** h_v4si:
**	movi	v([0-9]+).2d, 0xffffffff
**	and	v0.16b, (?:v0.16b, v\1.16b|v\1.16b, v0.16b)
**	ret
*/
v4si
h_v4si (v4si x)
{
  return __builtin_shuffle (x, (v4si){ 0, 0, 0, 0 }, (v4si){ 7, 1, 6, 3 });
}

/*
** f_v4sf:
**	movi	v([0-9]+).2d, 0xffffffff00000000
**	and	v0.16b, (?:v0.16b, v\1.16b|v\1.16b, v0.16b)
**	ret
*/
v4sf
f_v4sf (v4sf x)
{
  return __builtin_shuffle (x, (v4sf){ 0, 0, 0, 0 }, (v4si){ 0, 6, 2, 7 });
}

/*
** f_v8qi:
**	movi	d([0-9]+), 0xff00ff00ff000000
**	and	v0.8b, (?:v0.8b, v\1.8b|v\1.8b, v0.8b)
**	ret
*/
v8qi
f_v8qi (v8qi x)
{
  return __builtin_shuffle (x, (v8qi){ 0, 0, 0, 0, 0, 0, 0, 0 },
			    (v8qi){ 0, 8, 2, 9, 4, 10, 12, 11 });
}

/*
** f_v16qi:
**	...
**	and	v0.16b, (?:v0.16b, v[0-9]+.16b|v[0-9]+.16b, v0.16b)
**	ret
*/
v16qi
f_v16qi (v16qi x)
{
  return __builtin_shuffle (
      x, (v16qi){ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 },
      (v16qi){ 16, 1, 17, 3, 18, 5, 19, 7, 20, 9, 21, 11, 22, 13, 23, 24 });
}
