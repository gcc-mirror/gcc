/* { dg-do compile { target { ! riscv_abi_e } } } */
/* { dg-require-effective-target rv32 } */
/* { dg-options "-march=rv32gc" } */
/* { dg-skip-if "" { *-*-* } {"-O0" "-Os" "-Og" "-Oz" "-flto" } } */

#include "extend-shift-helpers.h"

/* { dg-final { scan-assembler "slli\ta\[0-9\],a\[0-9\],15" } } */
/* { dg-final { scan-assembler "srai\ta\[0-9\],a\[0-9\],24" } } */
SLONG_EXT_SCHAR_RSHIFT_N_SLONG(9)

/* { dg-final { scan-assembler "slli\ta\[0-9\],a\[0-9\],2" } } */
/* { dg-final { scan-assembler "srai\ta\[0-9\],a\[0-9\],16" } } */
SLONG_EXT_SSHORT_RSHIFT_N_SLONG(14)

/* { dg-final { scan-assembler "srai\ta\[0-9\],a\[0-9\],23" } } */
SLONG_EXT_SINT_RSHIFT_N_SLONG(23)
