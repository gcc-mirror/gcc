/* { dg-do compile } */
/* { dg-options "-O2 -mlittle-endian" } */
/* { dg-final { check-function-bodies "**" "" "" } } */

#pragma GCC target ("arch=armv8.2-a+fp16")

typedef __fp16 v4hf __attribute__ ((vector_size (8)));
typedef __fp16 v8hf __attribute__ ((vector_size (16)));
typedef __bf16 v4bf __attribute__ ((vector_size (8)));
typedef __bf16 v8bf __attribute__ ((vector_size (16)));
typedef short v4hi __attribute__ ((vector_size (8)));
typedef short v8hi __attribute__ ((vector_size (16)));

/*
** f_v4hf:
**	fmov	h0, h0
**	ret
*/
v4hf
f_v4hf (v4hf x)
{
  return __builtin_shuffle (x, (v4hf){ 0, 0, 0, 0 }, (v4hi){ 0, 4, 5, 6 });
}

/*
** g_v4hf:
**	fmov	h0, h0
**	ret
*/
v4hf
g_v4hf (v4hf x)
{
  return __builtin_shuffle ((v4hf){ 0, 0, 0, 0 }, x, (v4hi){ 4, 0, 1, 2 });
}

/*
** h_v4hf:
**	fmov	s0, s0
**	ret
*/
v4hf
h_v4hf (v4hf x)
{
  return __builtin_shuffle (x, (v4hf){ 0, 0, 0, 0 }, (v4hi){ 0, 1, 4, 5 });
}

/*
** f_v8hf:
**	fmov	h0, h0
**	ret
*/
v8hf
f_v8hf (v8hf x)
{
  return __builtin_shuffle (x, (v8hf){ 0, 0, 0, 0, 0, 0, 0, 0 },
			    (v8hi){ 0, 8, 9, 10, 11, 12, 13, 14 });
}

/*
** g_v8hf:
**	fmov	h0, h0
**	ret
*/
v8hf
g_v8hf (v8hf x)
{
  return __builtin_shuffle ((v8hf){ 0, 0, 0, 0, 0, 0, 0, 0 }, x,
			    (v8hi){ 8, 0, 1, 2, 3, 4, 5, 6 });
}

/*
** h_v8hf:
**	fmov	s0, s0
**	ret
*/
v8hf
h_v8hf (v8hf x)
{
  return __builtin_shuffle (x, (v8hf){ 0, 0, 0, 0, 0, 0, 0, 0 },
			    (v8hi){ 0, 1, 8, 9, 10, 11, 12, 13 });
}

/*
** f_v4bf:
**	fmov	h0, h0
**	ret
*/
v4bf
f_v4bf (v4bf x)
{
  return __builtin_shuffle (x, (v4bf){ 0, 0, 0, 0 }, (v4hi){ 0, 4, 5, 6 });
}

/*
** g_v4bf:
**	fmov	h0, h0
**	ret
*/
v4bf
g_v4bf (v4bf x)
{
  return __builtin_shuffle ((v4bf){ 0, 0, 0, 0 }, x, (v4hi){ 4, 0, 1, 2 });
}

/*
** h_v4bf:
**	fmov	s0, s0
**	ret
*/
v4bf
h_v4bf (v4bf x)
{
  return __builtin_shuffle (x, (v4bf){ 0, 0, 0, 0 }, (v4hi){ 0, 1, 4, 5 });
}

/*
** f_v8bf:
**	fmov	h0, h0
**	ret
*/
v8bf
f_v8bf (v8bf x)
{
  return __builtin_shuffle (x, (v8bf){ 0, 0, 0, 0, 0, 0, 0, 0 },
			    (v8hi){ 0, 8, 9, 10, 11, 12, 13, 14 });
}

/*
** g_v8bf:
**	fmov	h0, h0
**	ret
*/
v8bf
g_v8bf (v8bf x)
{
  return __builtin_shuffle ((v8bf){ 0, 0, 0, 0, 0, 0, 0, 0 }, x,
			    (v8hi){ 8, 0, 1, 2, 3, 4, 5, 6 });
}

/*
** h_v8bf:
**	fmov	s0, s0
**	ret
*/
v8bf
h_v8bf (v8bf x)
{
  return __builtin_shuffle (x, (v8bf){ 0, 0, 0, 0, 0, 0, 0, 0 },
			    (v8hi){ 0, 1, 8, 9, 10, 11, 12, 13 });
}
