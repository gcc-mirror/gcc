/* { dg-do compile } */
/* { dg-options "-O2 -mlittle-endian" } */
/* { dg-final { check-function-bodies "**" "" "" } } */

#pragma GCC target ("arch=armv8.2-a+fp16")

typedef short v4hi __attribute__ ((vector_size (8)));
typedef char v8qi __attribute__ ((vector_size (8)));
typedef short v8hi __attribute__ ((vector_size (16)));
typedef char v16qi __attribute__ ((vector_size (16)));

/*
** f_v4hi:
**	fmov	h0, h0
**	ret
*/
v4hi
f_v4hi (v4hi x)
{
  return __builtin_shuffle (x, (v4hi){ 0, 0, 0, 0 }, (v4hi){ 0, 4, 5, 6 });
}

/*
** g_v4hi:
**	fmov	h0, h0
**	ret
*/
v4hi
g_v4hi (v4hi x)
{
  return __builtin_shuffle ((v4hi){ 0, 0, 0, 0 }, x, (v4hi){ 4, 0, 1, 2 });
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
			    (v8hi){ 0, 8, 9, 10, 11, 12, 13, 14 });
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
			    (v8hi){ 8, 0, 1, 2, 3, 4, 5, 6 });
}

/*
** f_v8qi:
**	fmov	h0, h0
**	ret
*/
v8qi
f_v8qi (v8qi x)
{
  return __builtin_shuffle (x, (v8qi){ 0, 0, 0, 0, 0, 0, 0, 0 },
			    (v8qi){ 0, 1, 8, 9, 10, 11, 12, 13 });
}


/*
** g_v8qi:
**	fmov	h0, h0
**	ret
*/
v8qi
g_v8qi (v8qi x)
{
  return __builtin_shuffle ((v8qi){ 0, 0, 0, 0, 0, 0, 0, 0 }, x,
			    (v8qi){ 8, 9, 0, 1, 2, 3, 4, 5 });
}

/*
** h_v16qi:
**	fmov	h0, h0
**	ret
*/
v16qi
h_v16qi (v16qi x)
{
  return __builtin_shuffle (
      x, (v16qi){ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 },
      (v16qi){ 0, 1, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29 });
}
