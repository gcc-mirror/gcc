/* { dg-do compile { target { ! riscv_abi_e } } } */
/* { dg-require-effective-target rv32 } */
/* { dg-options "-march=rv32gc" } */
/* { dg-skip-if "" { *-*-* } {"-O0" "-Os" "-Og" "-Oz" "-flto" } } */

#include "extend-shift-helpers.h"

/* { dg-final { scan-assembler "slli\ta\[0-9\],a\[0-9\],2" } } */
/* { dg-final { scan-assembler "srli\ta\[0-9\],a\[0-9\],16" } } */
ULONG_EXT_USHORT_RSHIFT_N_ULONG(14)

/* { dg-final { scan-assembler "srli\ta\[0-9\],a\[0-9\],23" } } */
ULONG_EXT_UINT_RSHIFT_N_ULONG(23)
