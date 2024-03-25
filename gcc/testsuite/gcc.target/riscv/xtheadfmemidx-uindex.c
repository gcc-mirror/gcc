/* { dg-do compile } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-O1" "-Og" } } */
/* { dg-options "-march=rv64gc_xtheadmemidx_xtheadfmemidx -mabi=lp64d" { target { rv64 } } } */
/* { dg-options "-march=rv32imafc_xtheadmemidx_xtheadfmemidx -mabi=ilp32f" { target { rv32 } } } */

#include "xtheadmemidx-helpers.h"

LRU_REG_IMM(float, 0)
/* { dg-final { scan-assembler-times {\mth\.flurw\t[^\n\r]*0\M} 1 { target { rv64 } } } } */
/* { dg-final { scan-assembler-times {\mth\.flrw\t[^\n\r]*0\M} 1 { target { rv32 } } } } */
#if __riscv_xlen == 64
LRU_REG_IMM(double, 2)
/* { dg-final { scan-assembler-times {\mth\.flurd\t[^\n\r]*2\M} 1 { target { rv64 } } } } */
#endif

SRU_REG_IMM(float, 1)
/* { dg-final { scan-assembler-times {\mth\.fsurw\t[^\n\r]*1\M} 1 { target { rv64 } } } } */
/* { dg-final { scan-assembler-times {\mth\.fsrw\t[^\n\r]*1\M} 1 { target { rv32 } } } } */
#if __riscv_xlen == 64
SRU_REG_IMM(double, 3)
/* { dg-final { scan-assembler-times {\mth\.fsurd\t[^\n\r]*3\M} 1 { target { rv64 } } } } */
#endif

/* { dg-final { scan-assembler-not {\mslli\M} } } */

