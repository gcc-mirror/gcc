/* { dg-do compile { target { ! riscv_abi_e } } } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-O1" "-Og" } } */
/* { dg-options "-march=rv64gc_xtheadmemidx" { target { rv64 } } } */
/* { dg-options "-march=rv32gc_xtheadmemidx" { target { rv32 } } } */

#include "xtheadmemidx-helpers.h"

LR_REG_IMM(int8_t, 0)
/* { dg-final { scan-assembler-times {\mth.lrb\t[^\n\r]*0\M} 1 } } */
LR_REG_IMM(uint8_t, 1)
/* { dg-final { scan-assembler-times {\mth.lrbu\t[^\n\r]*1\M} 1 } } */
LR_REG_IMM(int16_t, 2)
/* { dg-final { scan-assembler-times {\mth.lrh\t[^\n\r]*2\M} 1 } } */
LR_REG_IMM(uint16_t, 3)
/* { dg-final { scan-assembler-times {\mth.lrhu\t[^\n\r]*3\M} 1 } } */
LR_REG_IMM(int32_t, 0)
/* { dg-final { scan-assembler-times {\mth.lrw\t[^\n\r]*0\M} 1 } } */
#if __riscv_xlen == 64
LR_REG_IMM(uint32_t, 1)
/* { dg-final { scan-assembler-times {\mth.lrwu\t[^\n\r]*1\M} 1 { target { rv64 } } } } */
LR_REG_IMM(int64_t, 2)
/* { dg-final { scan-assembler-times {\mth.lrd\t[^\n\r]*2\M} 1 { target { rv64 } } } } */
#endif

SR_REG_IMM(int8_t, 3)
/* { dg-final { scan-assembler-times {\mth.srb\t[^\n\r]*3\M} 1 } } */
SR_REG_IMM(int16_t, 0)
/* { dg-final { scan-assembler-times {\mth.srh\t[^\n\r]*0\M} 1 } } */
SR_REG_IMM(int32_t, 1)
/* { dg-final { scan-assembler-times {\mth.srw\t[^\n\r]*1\M} 1 } } */
#if __riscv_xlen == 64
SR_REG_IMM(int64_t, 2)
/* { dg-final { scan-assembler-times {\mth.srd\t[^\n\r]*2\M} 1 { target { rv64 } } } } */
#endif

/* { dg-final { scan-assembler-not {\mslli\M} } } */
