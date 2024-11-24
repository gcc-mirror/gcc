/* { dg-do compile { target { ! riscv_abi_e } } } */
/* { dg-require-effective-target rv64 } */
/* { dg-options "-march=rv64gc" } */
/* { dg-skip-if "" { *-*-* } {"-O0" "-Os" "-Og" "-Oz" "-flto" } } */

#include "extend-shift-helpers.h"

/* { dg-final { scan-assembler "slli\ta\[0-9\],a\[0-9\],34" } } */
/* { dg-final { scan-assembler "srli\ta\[0-9\],a\[0-9\],48" } } */
ULONG_EXT_USHORT_RSHIFT_N_ULONG(14)

/* { dg-final { scan-assembler "srli\ta\[0-9\],a\[0-9\],51" } } */
ULONG_EXT_USHORT_RSHIFT_N_ULONG(51)

/* { dg-final { scan-assembler "slli\ta\[0-9\],a\[0-9\],9" } } */
/* { dg-final { scan-assembler "srli\ta\[0-9\],a\[0-9\],32" } } */
ULONG_EXT_UINT_RSHIFT_N_ULONG(23)
