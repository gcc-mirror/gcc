/* { dg-do compile } */
/* { dg-options "-O2 -march=z13 -mzarch" } */
/* { dg-final { check-function-bodies "**" "" "" } } */

typedef signed char v2qi __attribute__ ((vector_size (2)));
typedef short v2hi __attribute__ ((vector_size (4)));
typedef int v2si __attribute__ ((vector_size (8)));
typedef long long v2di __attribute__ ((vector_size (16)));

typedef unsigned char uv2qi __attribute__ ((vector_size (2)));
typedef unsigned short uv2hi __attribute__ ((vector_size (4)));
typedef unsigned int uv2si __attribute__ ((vector_size (8)));
typedef unsigned long long uv2di __attribute__ ((vector_size (16)));

/*
** extendv2qiv2hi2:
**     vuphb	%v24,%v24
**     br	%r14
*/

v2hi extendv2qiv2hi2 (v2qi x)
{
  return __builtin_convertvector (x, v2hi);
}

/*
** extendv2hiv2si2:
**     vuphh	%v24,%v24
**     br	%r14
*/

v2si extendv2hiv2si2 (v2hi x)
{
  return __builtin_convertvector (x, v2si);
}

/*
** extendv2siv2di2:
**     vuphf	%v24,%v24
**     br	%r14
*/

v2di extendv2siv2di2 (v2si x)
{
  return __builtin_convertvector (x, v2di);
}

/*
** extenduv2qiuv2hi2:
**     vuplhb	%v24,%v24
**     br	%r14
*/

uv2hi extenduv2qiuv2hi2 (uv2qi x)
{
  return __builtin_convertvector (x, uv2hi);
}

/*
** extenduv2hiuv2si2:
**     vuplhh	%v24,%v24
**     br	%r14
*/

uv2si extenduv2hiuv2si2 (uv2hi x)
{
  return __builtin_convertvector (x, uv2si);
}

/*
** extenduv2siuv2di2:
**     vuplhf	%v24,%v24
**     br	%r14
*/

uv2di extenduv2siuv2di2 (uv2si x)
{
  return __builtin_convertvector (x, uv2di);
}
