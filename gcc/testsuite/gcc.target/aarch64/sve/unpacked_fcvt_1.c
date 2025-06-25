/* { dg-do compile } */
/* { dg-options "-O2 -msve-vector-bits=2048 -fno-schedule-insns -fno-schedule-insns2" } */

typedef _Float16 v32hf __attribute__((vector_size(64)));
typedef _Float16 v64hf __attribute__((vector_size(128)));

typedef float v32sf __attribute__((vector_size(128)));
typedef float v64sf __attribute__((vector_size(256)));

typedef double v32df __attribute__((vector_size(256)));

/*
** trunc_2sf2df:
**	ptrue	(p[0-7])\.b, vl256
**	ld1d	(z[0-9]+)\.d, \1/z, \[x0\]
**	fcvt	(z[0-9]+)\.s, \1/m, \2\.d
**	...
*/
v32sf
trunc_2sf2df (v32df x)
{
  return __builtin_convertvector (x, v32sf);
}

/*
** trunc_2hf2df:
**	ptrue	(p[0-7])\.b, vl256
**	ld1d	(z[0-9]+)\.d, \1/z, \[x0\]
**	fcvt	(z[0-9]+)\.h, \1/m, \2\.d
**	...
*/
v32hf
trunc_2hf2df (v32df x)
{
  return __builtin_convertvector (x, v32hf);
}

/*
** trunc_4hf4sf:
**	ptrue	(p[0-7])\.b, vl256
**	ld1w	(z[0-9]+)\.s, \1/z, \[x0\]
**	fcvt	(z[0-9]+)\.h, \1/m, \2\.s
**	...
*/
v64hf
trunc_4hf4sf (v64sf x)
{
  return __builtin_convertvector (x, v64hf);
}

/*
** trunc_2hf2sf:
**	...
**	ld1w	(z[0-9]+)\.d, p[0-7]/z, \[x0\]
**	ptrue	(p[0-7])\.d, vl32
**	fcvt	(z[0-9]+)\.h, \2/m, \1\.s
**	...
*/
v32hf
trunc_2hf2sf (v32sf x)
{
  return __builtin_convertvector (x, v32hf);
}

/*
** extend_2df2hf:
**	ptrue	(p[0-7])\.b, vl256
**	ld1h	(z[0-9]+)\.d, \1/z, \[x0\]
**	fcvt	(z[0-9]+)\.d, \1/m, \2\.h
**	...
*/
v32df
extend_2df2hf (v32hf x)
{
  return __builtin_convertvector (x, v32df);
}

/*
** extend_2df2sf:
**	ptrue	(p[0-7])\.b, vl256
**	ld1w	(z[0-9]+)\.d, \1/z, \[x0\]
**	fcvt	(z[0-9]+)\.d, \1/m, \2\.s
**	...
*/
v32df
extend_2df2sf (v32sf x)
{
  return __builtin_convertvector (x, v32df);
}

/*
** extend_4sf4hf:
**	ptrue	(p[0-7])\.b, vl256
**	ld1h	(z[0-9]+)\.s, \1/z, \[x0\]
**	fcvt	(z[0-9]+)\.s, \1/m, \2\.h
**	...
*/
v64sf
extend_4sf4hf (v64hf x)
{
  return __builtin_convertvector (x, v64sf);
}

/*
** extend_2sf2hf:
**	...
**	ld1h	(z[0-9]+)\.d, p[0-7]/z, \[x0\]
**	ptrue	(p[0-7])\.d, vl32
**	fcvt	(z[0-9]+)\.s, \2/m, \1\.h
**	...
*/
v32sf
extend_2sf2hf (v32hf x)
{
  return __builtin_convertvector (x, v32sf);
}

/* { dg-final { check-function-bodies "**" "" ""} } */
