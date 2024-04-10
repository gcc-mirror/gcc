/* { dg-do compile } */
/* { dg-options "-mrvv-vector-bits=scalable -march=rv64gcv -mabi=lp64d -fno-schedule-insns -fno-schedule-insns2" } */

#include "riscv_vector.h"

void func(unsigned char *out, unsigned char *in, unsigned long len) {
  unsigned long i = 0;
  while (i < len) {
    unsigned long vl = __riscv_vsetvl_e8m1(len - i);
    vuint8m1_t r = __riscv_vle8_v_u8m1(in + i, vl);
    __riscv_vse8_v_u8m1(out + i, r, vl);
    i += vl;
  }
/* { dg-final { scan-assembler-times {vsetvli} 1 { target { no-opts "-O0" no-opts "-g" no-opts "-funroll-loops" } } } } */}
