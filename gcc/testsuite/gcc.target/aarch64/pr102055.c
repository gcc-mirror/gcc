/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-final { check-function-bodies "**" "" "" } } */

#define vector __attribute__ ((vector_size (16)))

/*
** f:
**	rev64	v([0-9]+).16b, v0.16b
**	ext	v0.16b, v\1.16b, v\1.16b, #8
**	ret
*/
vector char
f (vector char a)
{
  return __builtin_shuffle (a, (vector char){ 15, 14, 13, 12, 11, 10, 9, 8,
					      7, 6, 5, 4, 3, 2, 1, 0 });
}

/*
** f1:
**	rev64	v([0-9]+).8h, v0.8h
**	ext	v0.16b, v\1.16b, v\1.16b, #8
**	ret
*/
vector short
f1 (vector short a)
{
  return __builtin_shuffle (a, (vector short){ 7, 6, 5, 4, 3, 2, 1, 0 });
}

/*
** f2:
**	rev64	v([0-9]+).4s, v0.4s
**	ext	v0.16b, v\1.16b, v\1.16b, #8
**	ret
*/
vector int
f2 (vector int a)
{
  return __builtin_shuffle (a, (vector int){ 3, 2, 1, 0 });
}
