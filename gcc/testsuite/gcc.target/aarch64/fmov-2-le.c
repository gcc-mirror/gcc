/* { dg-do compile } */
/* { dg-options "-O2 -mlittle-endian" } */
/* { dg-final { check-function-bodies "**" "" "" } } */

#pragma GCC target ("arch=armv8.2-a+fp16")

typedef int v2si __attribute__ ((vector_size (8)));
typedef short v4hi __attribute__ ((vector_size (8)));
typedef char v8qi __attribute__ ((vector_size (8)));
typedef long v2di __attribute__ ((vector_size (16)));
typedef int v4si __attribute__ ((vector_size (16)));
typedef short v8hi __attribute__ ((vector_size (16)));
typedef char v16qi __attribute__ ((vector_size (16)));

/*
** f_v2di:
**	fmov	h0, h0
**	ret
*/
v2di
f_v2di (v2di x)
{
  return x & (v2di){ 0xffff, 0 };
}

/*
** f_v4si:
**	fmov	h0, h0
**	ret
*/
v4si
f_v4si (v4si x)
{
  return x & (v4si){ 0xffff, 0, 0, 0 };
}

/*
** f_v2si:
**	fmov	h0, h0
**	ret
*/
v2si
f_v2si (v2si x)
{
  return x & (v2si){ 0xffff, 0 };
}

/*
** f_v8hi:
**	fmov	h0, h0
**	ret
*/
v8hi
f_v8hi (v8hi x)
{
  return x & (v8hi){ 0xffff, 0, 0, 0, 0, 0, 0, 0 };
}

/*
** f_v4hi:
**	fmov	h0, h0
**	ret
*/
v4hi
f_v4hi (v4hi x)
{
  return x & (v4hi){ 0xffff, 0, 0, 0 };
}

/*
** f_v16qi:
**	fmov	h0, h0
**	ret
*/
v16qi
f_v16qi (v16qi x)
{
  return x & (v16qi){ 0xff, 0xff, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 };
}

/*
** f_v8qi:
**	fmov	h0, h0
**	ret
*/
v8qi
f_v8qi (v8qi x)
{
  return x & (v8qi){ 0xff, 0xff, 0, 0, 0, 0, 0, 0 };
}
