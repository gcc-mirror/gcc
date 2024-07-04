/* { dg-do compile } */
/* { dg-options "-O2 -march=z13 -mzarch" } */
/* { dg-final { check-function-bodies "**" "" "" } } */

typedef signed char v4qi __attribute__ ((vector_size (4)));
typedef short v4hi __attribute__ ((vector_size (8)));
typedef int v4si __attribute__ ((vector_size (16)));

typedef unsigned char uv4qi __attribute__ ((vector_size (4)));
typedef unsigned short uv4hi __attribute__ ((vector_size (8)));
typedef unsigned int uv4si __attribute__ ((vector_size (16)));

/*
** extendv4qiv4hi2:
**     vuphb	%v24,%v24
**     br	%r14
*/

v4hi extendv4qiv4hi2 (v4qi x)
{
  return __builtin_convertvector (x, v4hi);
}

/*
** extendv4hiv4si2:
**     vuphh	%v24,%v24
**     br	%r14
*/

v4si extendv4hiv4si2 (v4hi x)
{
  return __builtin_convertvector (x, v4si);
}

/*
** extenduv4qiuv4hi2:
**     vuplhb	%v24,%v24
**     br	%r14
*/

uv4hi extenduv4qiuv4hi2 (uv4qi x)
{
  return __builtin_convertvector (x, uv4hi);
}

/*
** extenduv4hiuv4si2:
**     vuplhh	%v24,%v24
**     br	%r14
*/

uv4si extenduv4hiuv4si2 (uv4hi x)
{
  return __builtin_convertvector (x, uv4si);
}
