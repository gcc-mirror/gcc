/* { dg-do compile } */
/* { dg-options "-O2 -mbig-endian" } */
/* { dg-final { check-function-bodies "**" "" "" } } */

#pragma GCC target ("arch=armv8.2-a+fp16")

typedef short v4hi __attribute__ ((vector_size (8)));
typedef short v8hi __attribute__ ((vector_size (16)));

/*
** f_v4hi:
**	fmov	h0, h0
**	ret
*/
v4hi
f_v4hi (v4hi x)
{
  return __builtin_shuffle (x, (v4hi){ 0, 0, 0, 0 }, (v4hi){ 4, 5, 6, 3 });
}

/*
** g_v4hi:
**	fmov	h0, h0
**	ret
*/
v4hi
g_v4hi (v4hi x)
{
  return __builtin_shuffle ((v4hi){ 0, 0, 0, 0 }, x, (v4hi){ 0, 1, 2, 7 });
}

/*
** f_v8hi:
**	fmov	h0, h0
**	ret
*/
v8hi
f_v8hi (v8hi x)
{
  return __builtin_shuffle (x, (v8hi){ 0, 0, 0, 0, 0, 0, 0, 0 },
			    (v8hi){ 8, 9, 10, 11, 12, 13, 14, 7 });
}

/*
** g_v8hi:
**	fmov	h0, h0
**	ret
*/
v8hi
g_v8hi (v8hi x)
{
  return __builtin_shuffle ((v8hi){ 0, 0, 0, 0, 0, 0, 0, 0 }, x,
			    (v8hi){ 0, 1, 2, 3, 4, 5, 6, 15 });
}
