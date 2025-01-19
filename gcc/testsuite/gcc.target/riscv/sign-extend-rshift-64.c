/* { dg-do compile { target { ! riscv_abi_e } } } */
/* { dg-require-effective-target rv64 } */
/* { dg-options "-march=rv64gc" } */
/* { dg-skip-if "" { *-*-* } {"-O0" "-Os" "-Og" "-Oz" "-flto" } } */

#include "extend-shift-helpers.h"

/* { dg-final { scan-assembler "slli\ta\[0-9\],a\[0-9\],47" } } */
/* { dg-final { scan-assembler "srai\ta\[0-9\],a\[0-9\],56" } } */
SLONG_EXT_SCHAR_RSHIFT_N_SLONG(9)

/* { dg-final { scan-assembler "slli\ta\[0-9\],a\[0-9\],34" } } */
/* { dg-final { scan-assembler "srai\ta\[0-9\],a\[0-9\],48" } } */
SLONG_EXT_SSHORT_RSHIFT_N_SLONG(14)

/* { dg-final { scan-assembler "srai\ta\[0-9\],a\[0-9\],51" } } */
SLONG_EXT_SSHORT_RSHIFT_N_SLONG(51)
