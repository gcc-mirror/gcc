/* { dg-do compile } */
/* { dg-options "-march=rv32gcv -mabi=ilp32d -O3" } */
/* { dg-final { check-function-bodies "**" "" } } */

#include "riscv_vector.h"

/*
** mov4:
**	vl1re64\.v\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),0\s*\([a-x0-9]+\)
**	vs1r\.v\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),0\s*\([a-x0-9]+\)
**  ret
*/
void mov4 (int64_t *in, int64_t *out)
{
  vint64m1_t v = *(vint64m1_t*)in;
  *(vint64m1_t*)out = v;
}

/*
** mov5:
**	vl2re64\.v\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),0\s*\([a-x0-9]+\)
**	vs2r\.v\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),0\s*\([a-x0-9]+\)
**  ret
*/
void mov5 (int64_t *in, int64_t *out)
{
  vint64m2_t v = *(vint64m2_t*)in;
  *(vint64m2_t*)out = v;
}

/*
** mov6:
**	vl4re64\.v\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),0\s*\([a-x0-9]+\)
**	vs4r\.v\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),0\s*\([a-x0-9]+\)
**  ret
*/
void mov6 (int64_t *in, int64_t *out)
{
  vint64m4_t v = *(vint64m4_t*)in;
  *(vint64m4_t*)out = v;
}

/*
** mov7:
**	vl8re64\.v\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),0\s*\([a-x0-9]+\)
**	vs8r\.v\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),0\s*\([a-x0-9]+\)
**  ret
*/
void mov7 (int64_t *in, int64_t *out)
{
  vint64m8_t v = *(vint64m8_t*)in;
  *(vint64m8_t*)out = v;
}

/*
** mov8:
**	vl1re64\.v\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),0\s*\([a-x0-9]+\)
**	vs1r\.v\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),0\s*\([a-x0-9]+\)
**  ret
*/
void mov8 (uint64_t *in, uint64_t *out)
{
  vuint64m1_t v = *(vuint64m1_t*)in;
  *(vuint64m1_t*)out = v;
}

/*
** mov9:
**	vl2re64\.v\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),0\s*\([a-x0-9]+\)
**	vs2r\.v\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),0\s*\([a-x0-9]+\)
**  ret
*/
void mov9 (uint64_t *in, uint64_t *out)
{
  vuint64m2_t v = *(vuint64m2_t*)in;
  *(vuint64m2_t*)out = v;
}

/*
** mov10:
**	vl4re64\.v\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),0\s*\([a-x0-9]+\)
**	vs4r\.v\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),0\s*\([a-x0-9]+\)
**  ret
*/
void mov10 (uint64_t *in, uint64_t *out)
{
  vuint64m4_t v = *(vuint64m4_t*)in;
  *(vuint64m4_t*)out = v;
}

/*
** mov11:
**	vl8re64\.v\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),0\s*\([a-x0-9]+\)
**	vs8r\.v\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),0\s*\([a-x0-9]+\)
**  ret
*/
void mov11 (uint64_t *in, uint64_t *out)
{
  vuint64m8_t v = *(vuint64m8_t*)in;
  *(vuint64m8_t*)out = v;
}
