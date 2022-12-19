/* { dg-do compile } */
/* { dg-options "-march=rv32gcv -mabi=ilp32d -O3 -fno-schedule-insns -fno-schedule-insns2" } */
/* { dg-final { check-function-bodies "**" "" } } */

#include "riscv_vector.h"

/* Test tieable of RVV types with same LMUL.  */
/*
** mov1:
**	vsetvli\s+[a-x0-9]+,\s*zero,\s*e8,\s*mf2,\s*t[au],\s*m[au]
**	vle8\.v\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),0\s*\([a-x0-9]+\)
**	vse8\.v\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),0\s*\([a-x0-9]+\)
**  addi\t[a-x0-9]+,[a-x0-9]+,1
**	vse8\.v\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),0\s*\([a-x0-9]+\)
**  addi\t[a-x0-9]+,[a-x0-9]+,2
**	vse8\.v\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),0\s*\([a-x0-9]+\)
**  ret
*/
void mov1 (int8_t *in, int8_t *out, int M)
{
  vint8mf2_t v1 = *(vint8mf2_t*)(in);
  vint16mf2_t v2 = *(vint16mf2_t*)(in);
  vint32mf2_t v3 = *(vint32mf2_t*)(in);
  *(vint8mf2_t*)(out) = v1;
  *(vint16mf2_t*)(out + 1) = v2;
  *(vint32mf2_t*)(out + 2) = v3;
}

/*
** mov2:
**	vsetvli\s+[a-x0-9]+,\s*zero,\s*e8,\s*mf4,\s*t[au],\s*m[au]
**	vle8\.v\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),0\s*\([a-x0-9]+\)
**	vse8\.v\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),0\s*\([a-x0-9]+\)
**  addi\t[a-x0-9]+,[a-x0-9]+,1
**	vse8\.v\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),0\s*\([a-x0-9]+\)
**  ret
*/
void mov2 (int8_t *in, int8_t *out, int M)
{
  vint8mf4_t v1 = *(vint8mf4_t*)(in);
  vint16mf4_t v2 = *(vint16mf4_t*)(in);
  *(vint8mf4_t*)(out) = v1;
  *(vint16mf4_t*)(out + 1) = v2;
}
