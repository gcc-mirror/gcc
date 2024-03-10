/* { dg-do compile } */
/* { dg-options "-O3 -march=rv32gcv -mabi=ilp32d" } */

#include "riscv_vector.h"

void mov0 (int8_t *in, int8_t *out) 
{ 
  register vint8mf4x3_t v1 asm("v1") = *(vint8mf4x3_t*)in; 
  asm volatile ("# %0"::"vr"(v1)); 
  register vint8mf4x3_t v2 asm("v2") = v1; 
  *(vint8mf4x3_t*)out = v2; 
  asm volatile ("# %0"::"vr"(v2)); 
}

void mov1 (int8_t *in, int8_t *out) 
{ 
  register vint8mf4x3_t v1 asm("v2") = *(vint8mf4x3_t*)in; 
  asm volatile ("# %0"::"vr"(v1)); 
  register vint8mf4x3_t v2 asm("v1") = v1; 
  *(vint8mf4x3_t*)out = v2; 
  asm volatile ("# %0"::"vr"(v2)); 
}

/* { dg-final { scan-assembler-times {vmv1r\.v\tv4,v3} 1 } } */
/* { dg-final { scan-assembler-times {vmv1r\.v\tv3,v2} 1 } } */
/* { dg-final { scan-assembler-times {vmv1r\.v\tv2,v1} 1 } } */
/* { dg-final { scan-assembler-times {vmv1r\.v\tv1,v2} 1 } } */
/* { dg-final { scan-assembler-times {vmv1r\.v\tv2,v3} 1 } } */
/* { dg-final { scan-assembler-times {vmv1r\.v\tv3,v4} 1 } } */
