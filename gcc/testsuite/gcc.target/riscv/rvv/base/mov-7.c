/* { dg-do compile } */
/* { dg-options "-march=rv32gcv -mabi=ilp32d -O3" } */

#include "riscv_vector.h"

/* This testcase is testing whether RISC-V define REGMODE_NATURAL_SIZE.  */
void foo (int8_t *in, int8_t *out)
{
  vint8mf2_t v = *(vint8mf2_t*)in;
  vint32mf2_t v2 = *(vint32mf2_t*)in;
  *(vint8mf2_t*)out = v;
  *(vint32mf2_t*)(out + 16) = v2;
}
