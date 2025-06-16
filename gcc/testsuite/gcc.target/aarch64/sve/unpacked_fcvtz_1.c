/* { dg-do compile } */
/* { dg-options "-O2 -msve-vector-bits=2048 -fno-schedule-insns -fno-schedule-insns2" } */

#include <stdint.h>

typedef _Float16 v32hf __attribute__((vector_size(64)));
typedef _Float16 v64hf __attribute__((vector_size(128)));

typedef float v32sf __attribute__((vector_size(128)));
typedef float v64sf __attribute__((vector_size(256)));

typedef double v32df __attribute__((vector_size(256)));

typedef int16_t v32hi __attribute__((vector_size(64)));
typedef int16_t v64hi __attribute__((vector_size(128)));
typedef uint16_t v32uhi __attribute__((vector_size(64)));
typedef uint16_t v64uhi __attribute__((vector_size(128)));

typedef int32_t v32si __attribute__((vector_size(128)));
typedef int32_t v64si __attribute__((vector_size(256)));
typedef uint32_t v32usi __attribute__((vector_size(128)));
typedef uint32_t v64usi __attribute__((vector_size(256)));

typedef int64_t v32di __attribute__((vector_size(256)));
typedef uint64_t v32udi __attribute__((vector_size(256)));


/*
** fix_trunc_2hi2hf:
**	...
**	ld1h	(z[0-9]+)\.d, p[0-7]/z, \[x0\]
**	ptrue	(p[0-7])\.d, vl32
**	fcvtzs	(z[0-9]+)\.h, \2/m, \1\.h
**	...
*/
v32hi
fix_trunc_2hi2hf (v32hf x)
{
  return __builtin_convertvector (x, v32hi);
}

/*
** fix_trunc_2uhi2hf:
**	...
**	ld1h	(z[0-9]+)\.d, p[0-7]/z, \[x0\]
**	ptrue	(p[0-7])\.d, vl32
**	fcvtzu	(z[0-9]+)\.h, \2/m, \1\.h
**	...
*/
v32uhi
fix_trunc_2uhi2hf (v32hf x)
{
  return __builtin_convertvector (x, v32uhi);
}

/*
** fix_trunc_2si2hf:
**	...
**	ld1h	(z[0-9]+)\.d, p[0-7]/z, \[x0\]
**	ptrue	(p[0-7])\.d, vl32
**	fcvtzs	(z[0-9]+)\.s, \2/m, \1\.h
**	...
*/
v32si
fix_trunc_2si2hf (v32hf x)
{
  return __builtin_convertvector (x, v32si);
}

/*
** fix_trunc_2usi2hf:
**	...
**	ld1h	(z[0-9]+)\.d, p[0-7]/z, \[x0\]
**	ptrue	(p[0-7])\.d, vl32
**	fcvtzu	(z[0-9]+)\.s, \2/m, \1\.h
**	...
*/
v32usi
fix_trunc_2usi2hf (v32hf x)
{
  return __builtin_convertvector (x, v32usi);
}

/*
** fix_trunc_2di2hf:
**	ptrue	(p[0-7])\.b, vl256
**	ld1h	(z[0-9]+)\.d, \1/z, \[x0\]
**	fcvtzs	(z[0-9]+)\.d, \1/m, \2\.h
**	...
*/
v32di
fix_trunc_2di2hf (v32hf x)
{
  return __builtin_convertvector (x, v32di);
}

/*
** fix_trunc_2udi2hf:
**	ptrue	(p[0-7])\.b, vl256
**	ld1h	(z[0-9]+)\.d, \1/z, \[x0\]
**	fcvtzu	(z[0-9]+)\.d, \1/m, \2\.h
**	...
*/
v32udi
fix_trunc_2udi2hf (v32hf x)
{
  return __builtin_convertvector (x, v32udi);
}

/*
** fix_trunc_4hi4hf:
**	...
**	ld1h	(z[0-9]+)\.s, p[0-7]/z, \[x0\]
**	ptrue	(p[0-7])\.s, vl64
**	fcvtzs	(z[0-9]+)\.h, \2/m, \1\.h
**	...
*/
v64hi
fix_trunc_4hi4hf (v64hf x)
{
  return __builtin_convertvector (x, v64hi);
}

/*
** fix_trunc_4uhi4hf:
**	...
**	ld1h	(z[0-9]+)\.s, p[0-7]/z, \[x0\]
**	ptrue	(p[0-7])\.s, vl64
**	fcvtzu	(z[0-9]+)\.h, \2/m, \1\.h
**	...
*/
v64uhi
fix_trunc_4uhi4hf (v64hf x)
{
  return __builtin_convertvector (x, v64uhi);
}

/*
** fix_trunc_4si4hf:
**	ptrue	(p[0-7])\.b, vl256
**	ld1h	(z[0-9]+)\.s, \1/z, \[x0\]
**	fcvtzs	(z[0-9]+)\.s, \1/m, \2\.h
**	...
*/
v64si
fix_trunc_4si4hf (v64hf x)
{
  return __builtin_convertvector (x, v64si);
}

/*
** fix_trunc_4usi4hf:
**	ptrue	(p[0-7])\.b, vl256
**	ld1h	(z[0-9]+)\.s, \1/z, \[x0\]
**	fcvtzu	(z[0-9]+)\.s, \1/m, \2\.h
**	...
*/
v64usi
fix_trunc_4usi4hf (v64hf x)
{
  return __builtin_convertvector (x, v64usi);
}

/*
** fix_trunc_2si2sf:
**	...
**	ld1w	(z[0-9]+)\.d, p[0-7]/z, \[x0\]
**	ptrue	(p[0-7])\.d, vl32
**	fcvtzs	(z[0-9]+)\.s, \2/m, \1\.s
**	...
*/
v32si
fix_trunc_2si2sf (v32sf x)
{
  return __builtin_convertvector (x, v32si);
}

/*
** fix_trunc_2usi2sf:
**	...
**	ld1w	(z[0-9]+)\.d, p[0-7]/z, \[x0\]
**	ptrue	(p[0-7])\.d, vl32
**	fcvtzu	(z[0-9]+)\.s, \2/m, \1\.s
**	...
*/
v32usi
fix_trunc_2usi2sf (v32sf x)
{
  return __builtin_convertvector (x, v32usi);
}

/*
** fix_trunc_2di2sf:
**	ptrue	(p[0-7])\.b, vl256
**	ld1w	(z[0-9]+)\.d, \1/z, \[x0\]
**	fcvtzs	(z[0-9]+)\.d, \1/m, \2\.s
**	...
*/
v32di
fix_trunc_2di2sf (v32sf x)
{
  return __builtin_convertvector (x, v32di);
}

/*
** fix_trunc_2udi2sf:
**	ptrue	(p[0-7])\.b, vl256
**	ld1w	(z[0-9]+)\.d, \1/z, \[x0\]
**	fcvtzu	(z[0-9]+)\.d, \1/m, \2\.s
**	...
*/
v32udi
fix_trunc_2udi2sf (v32sf x)
{
  return __builtin_convertvector (x, v32udi);
}

/*
** fix_trunc_2si2df:
**	ptrue	(p[0-7])\.b, vl256
**	ld1d	(z[0-9]+)\.d, \1/z, \[x0\]
**	fcvtzs	(z[0-9]+)\.s, \1/m, \2\.d
**	...
*/
v32si
fix_trunc_2si2df (v32df x)
{
  return __builtin_convertvector (x, v32si);
}

/*
** fix_trunc_2usi2df:
**	ptrue	(p[0-7])\.b, vl256
**	ld1d	(z[0-9]+)\.d, \1/z, \[x0\]
**	fcvtzu	(z[0-9]+)\.s, \1/m, \2\.d
**	...
*/
v32usi
fix_trunc_2usi2df (v32df x)
{
  return __builtin_convertvector (x, v32usi);
}

/* { dg-final { check-function-bodies "**" "" ""} } */
