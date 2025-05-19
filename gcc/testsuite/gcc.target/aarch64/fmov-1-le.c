/* { dg-do compile } */
/* { dg-options "-O2 -mlittle-endian" } */
/* { dg-final { check-function-bodies "**" "" "" } } */

#pragma GCC target ("arch=armv8-a")

typedef int v2si __attribute__ ((vector_size (8)));
typedef float v2sf __attribute__ ((vector_size (8)));
typedef short v4hi __attribute__ ((vector_size (8)));
typedef char v8qi __attribute__ ((vector_size (8)));
typedef long v2di __attribute__ ((vector_size (16)));
typedef double v2df __attribute__ ((vector_size (16)));
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
  return x & (v4hi){ 0xffff, 0xffff, 0, 0 };
}

/*
** g_v4hi:
**	movi	d([0-9]+), 0xffff00000000ffff
**	and	v0.8b, (?:v0.8b, v\1.8b|v\1.8b, v0.8b)
**	ret
*/
v4hi
g_v4hi (v4hi x)
{
  return x & (v4hi){ 0xffff, 0, 0, 0xffff };
}

/*
** f_v8hi:
**	fmov	s0, s0
**	ret
*/
v8hi
f_v8hi (v8hi x)
{
  return x & (v8hi){ 0xffff, 0xffff, 0, 0, 0, 0, 0, 0 };
}

/*
** g_v8hi:
**	fmov	d0, d0
**	ret
*/
v8hi
g_v8hi (v8hi x)
{
  return x & (v8hi){ 0xffff, 0xffff, 0xffff, 0xffff, 0, 0, 0, 0 };
}

/*
** f_v2si:
**	fmov	s0, s0
**	ret
*/
v2si
f_v2si (v2si x)
{
  return x & (v2si){ 0xffffffff, 0 };
}

/*
** f_v2di:
**	fmov	d0, d0
**	ret
*/
v2di
f_v2di (v2di x)
{
  return x & (v2di){ 0xffffffffffffffff, 0 };
}

/*
** g_v2di:
**	fmov	s0, s0
**	ret
*/
v2di
g_v2di (v2di x)
{
  return x & (v2di){ 0xffffffff, 0 };
}

/*
** f_v4si:
**	fmov	s0, s0
**	ret
*/
v4si
f_v4si (v4si x)
{
  return x & (v4si){ 0xffffffff, 0, 0, 0 };
}

/*
** h_v4si:
**	fmov	d0, d0
**	ret
*/
v4si
h_v4si (v4si x)
{
  return x & (v4si){ 0xffffffff, 0xffffffff, 0, 0 };
}

/*
** f_v8qi:
**	fmov	s0, s0
**	ret
*/
v8qi
f_v8qi (v8qi x)
{
  return x & (v8qi){ 0xff, 0xff, 0xff, 0xff, 0, 0, 0, 0 };
}

/*
** f_v16qi:
**	fmov	d0, d0
**	ret
*/
v16qi
f_v16qi (v16qi x)
{
  return x & (v16qi){ 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
		      0,    0,    0,    0,    0,    0,    0,    0 };
}

/*
** g_v16qi:
**	fmov	s0, s0
**	ret
*/
v16qi
g_v16qi (v16qi x)
{
  return x & (v16qi){ 0xff, 0xff, 0xff, 0xff, 0, 0, 0, 0,
		      0,    0,    0,    0,    0, 0, 0, 0 };
}
