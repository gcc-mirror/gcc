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
** float_2hf2hi:
**	...
**	ld1h	(z[0-9]+)\.d, p[0-7]/z, \[x0\]
**	ptrue	(p[0-7])\.d, vl32
**	scvtf	(z[0-9]+)\.h, \2/m, \1\.h
**	...
*/
v32hf
float_2hf2hi (v32hi x)
{
  return __builtin_convertvector (x, v32hf);
}

/*
** float_2hf2uhi:
**	...
**	ld1h	(z[0-9]+)\.d, p[0-7]/z, \[x0\]
**	ptrue	(p[0-7])\.d, vl32
**	ucvtf	(z[0-9]+)\.h, \2/m, \1\.h
**	...
*/
v32hf
float_2hf2uhi (v32uhi x)
{
  return __builtin_convertvector (x, v32hf);
}

/*
** float_2hf2si:
**	...
**	ld1w	(z[0-9]+)\.d, p[0-7]/z, \[x0\]
**	ptrue	(p[0-7])\.d, vl32
**	scvtf	(z[0-9]+)\.h, \2/m, \1\.s
**	...
*/
v32hf
float_2hf2si (v32si x)
{
  return __builtin_convertvector (x, v32hf);
}

/*
** float_2hf2usi:
**	...
**	ld1w	(z[0-9]+)\.d, p[0-7]/z, \[x0\]
**	ptrue	(p[0-7])\.d, vl32
**	ucvtf	(z[0-9]+)\.h, \2/m, \1\.s
**	...
*/
v32hf
float_2hf2usi (v32usi x)
{
  return __builtin_convertvector (x, v32hf);
}

/*
** float_2hf2di:
**	ptrue	(p[0-7])\.b, vl256
**	ld1d	(z[0-9]+)\.d, \1/z, \[x0\]
**	scvtf	(z[0-9]+)\.h, \1/m, \2\.d
**	...
*/
v32hf
float_2hf2di (v32di x)
{
  return __builtin_convertvector (x, v32hf);
}

/*
** float_2hf2udi:
**	ptrue	(p[0-7])\.b, vl256
**	ld1d	(z[0-9]+)\.d, \1/z, \[x0\]
**	ucvtf	(z[0-9]+)\.h, \1/m, \2\.d
**	...
*/
v32hf
float_2hf2udi (v32udi x)
{
  return __builtin_convertvector (x, v32hf);
}

/*
** float_4hf4hi:
**	...
**	ld1h	(z[0-9]+)\.s, p[0-7]/z, \[x0\]
**	ptrue	(p[0-7])\.s, vl64
**	scvtf	(z[0-9]+)\.h, \2/m, \1\.h
**	...
*/
v64hf
float_4hf4hi (v64hi x)
{
  return __builtin_convertvector (x, v64hf);
}

/*
** float_4hf4uhi:
**	...
**	ld1h	(z[0-9]+)\.s, p[0-7]/z, \[x0\]
**	ptrue	(p[0-7])\.s, vl64
**	ucvtf	(z[0-9]+)\.h, \2/m, \1\.h
**	...
*/
v64hf
float_4hf4uhi (v64uhi x)
{
  return __builtin_convertvector (x, v64hf);
}

/*
** float_4hf4si:
**	ptrue	(p[0-7])\.b, vl256
**	ld1w	(z[0-9]+)\.s, \1/z, \[x0\]
**	scvtf	(z[0-9]+)\.h, \1/m, \2\.s
**	...
*/
v64hf
float_4hf4si (v64si x)
{
  return __builtin_convertvector (x, v64hf);
}

/*
** float_4hf4usi:
**	ptrue	(p[0-7])\.b, vl256
**	ld1w	(z[0-9]+)\.s, \1/z, \[x0\]
**	ucvtf	(z[0-9]+)\.h, \1/m, \2\.s
**	...
*/
v64hf
float_4hf4usi (v64usi x)
{
  return __builtin_convertvector (x, v64hf);
}

/*
** float_2sf2si:
**	...
**	ld1w	(z[0-9]+)\.d, p[0-7]/z, \[x0\]
**	ptrue	(p[0-7])\.d, vl32
**	scvtf	(z[0-9]+)\.s, \2/m, \1\.s
**	...
*/
v32sf
float_2sf2si (v32si x)
{
  return __builtin_convertvector (x, v32sf);
}

/*
** float_2sf2usi:
**	...
**	ld1w	(z[0-9]+)\.d, p[0-7]/z, \[x0\]
**	ptrue	(p[0-7])\.d, vl32
**	ucvtf	(z[0-9]+)\.s, \2/m, \1\.s
**	...
*/
v32sf
float_2sf2usi (v32usi x)
{
  return __builtin_convertvector (x, v32sf);
}

/*
** float_2sf2di:
**	ptrue	(p[0-7])\.b, vl256
**	ld1d	(z[0-9]+)\.d, \1/z, \[x0\]
**	scvtf	(z[0-9]+)\.s, \1/m, \2\.d
**	...
*/
v32sf
float_2sf2di (v32di x)
{
  return __builtin_convertvector (x, v32sf);
}

/*
** float_2sf2udi:
**	ptrue	(p[0-7])\.b, vl256
**	ld1d	(z[0-9]+)\.d, \1/z, \[x0\]
**	ucvtf	(z[0-9]+)\.s, \1/m, \2\.d
**	...
*/
v32sf
float_2sf2udi (v32udi x)
{
  return __builtin_convertvector (x, v32sf);
}

/* { dg-final { check-function-bodies "**" "" ""} } */
