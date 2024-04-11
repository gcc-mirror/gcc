/* { dg-do compile } */
/* { dg-options "-march=rv32gcv -mabi=ilp32 -mpreferred-stack-boundary=3 -fno-schedule-insns -fno-schedule-insns2 -O3 -mrvv-vector-bits=zvl" } */

#include "riscv_vector.h"

void f (char*);

void stack_check_alloca_1 (vuint8m1_t data, uint8_t *base, int y, ...)
{
  vuint8m8_t v0, v8, v16, v24;
  asm volatile ("nop"
                : "=vr" (v0), "=vr" (v8), "=vr" (v16), "=vr" (v24)
                :
                :);
  asm volatile ("nop"
                :
                : "vr" (v0), "vr" (v8), "vr" (v16), "vr" (v24)
                :);
  *(vuint8m1_t *)base = data;
  char* pStr = (char*)__builtin_alloca(y);
  f(pStr);
}

/* Compiler should not cause an ICE in this testcase.  */
