/* { dg-do compile { target { ! riscv_abi_e } } } */
/* { dg-add-options "riscv_v" } */
/* { dg-additional-options "-mrvv-vector-bits=scalable -fno-vect-cost-model -fdump-tree-vect-details" } */

#include <stdint-gcc.h>

void __attribute__ ((noipa))
popcount_32 (uint32_t *restrict dst, uint32_t *restrict src, int size)
{
  for (int i = 0; i < size; ++i)
    dst[i] = __builtin_popcount (src[i]);
}

void __attribute__ ((noipa))
popcount_64 (uint64_t *restrict dst, uint64_t *restrict src, int size)
{
  for (int i = 0; i < size; ++i)
    dst[i] = __builtin_popcountll (src[i]);
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops in function" 2 "vect" } } */
/* { dg-final { scan-assembler-times "vcpop.v" 2 { target { riscv_zvbb } } } } */
