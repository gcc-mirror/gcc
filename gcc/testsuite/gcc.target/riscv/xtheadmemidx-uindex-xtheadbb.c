/* { dg-do compile { target { ! riscv_abi_e } } } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-O1" "-Og" } } */
/* { dg-options "-march=rv64gc_xtheadbb_xtheadmemidx" { target { rv64 } } } */
/* { dg-options "-march=rv32gc_xtheadbb_xtheadmemidx" { target { rv32 } } } */

#include "xtheadmemidx-helpers.h"

LRU_REG_IMM(int8_t, 0)
/* { dg-final { scan-assembler-times {\mth.lurb\t[^\n\r]*0\M} 1 { target { rv64 } } } } */
/* { dg-final { scan-assembler-times {\mth.lrb\t[^\n\r]*0\M} 1 { target { rv32 } } } } */
LRU_REG_IMM(uint8_t, 1)
/* { dg-final { scan-assembler-times {\mth.lurbu\t[^\n\r]*1\M} 1 { target { rv64 } } } } */
/* { dg-final { scan-assembler-times {\mth.lrbu\t[^\n\r]*1\M} 1 { target { rv32 } } } } */
LRU_REG_IMM(int16_t, 2)
/* { dg-final { scan-assembler-times {\mth.lurh\t[^\n\r]*2\M} 1 { target { rv64 } } } } */
/* { dg-final { scan-assembler-times {\mth.lrh\t[^\n\r]*2\M} 1 { target { rv32 } } } } */
LRU_REG_IMM(uint16_t, 3)
/* { dg-final { scan-assembler-times {\mth.lurhu\t[^\n\r]*3\M} 1 { target { rv64 } } } } */
/* { dg-final { scan-assembler-times {\mth.lrhu\t[^\n\r]*3\M} 1 { target { rv32 } } } } */
LRU_REG_IMM(int32_t, 0)
/* { dg-final { scan-assembler-times {\mth.lurw\t[^\n\r]*0\M} 1 { target { rv64 } } } } */
/* { dg-final { scan-assembler-times {\mth.lrw\t[^\n\r]*0\M} 1 { target { rv32 } } } } */
#if __riscv_xlen == 64
LRU_REG_IMM(uint32_t, 1)
/* { dg-final { scan-assembler-times {\mth.lurwu\t[^\n\r]*1\M} 1 { target { rv64 } } } } */
LRU_REG_IMM(int64_t, 2)
/* { dg-final { scan-assembler-times {\mth.lurd\t[^\n\r]*2\M} 1 { target { rv64 } } } } */
#endif

SRU_REG_IMM(int8_t, 3)
/* { dg-final { scan-assembler-times {\mth.surb\t[^\n\r]*3\M} 1 { target { rv64 } } } } */
/* { dg-final { scan-assembler-times {\mth.srb\t[^\n\r]*3\M} 1 { target { rv32 } } } } */
SRU_REG_IMM(int16_t, 0)
/* { dg-final { scan-assembler-times {\mth.surh\t[^\n\r]*0\M} 1 { target { rv64 } } } } */
/* { dg-final { scan-assembler-times {\mth.srh\t[^\n\r]*0\M} 1 { target { rv32 } } } } */
SRU_REG_IMM(int32_t, 1)
/* { dg-final { scan-assembler-times {\mth.surw\t[^\n\r]*1\M} 1 { target { rv64 } } } } */
/* { dg-final { scan-assembler-times {\mth.srw\t[^\n\r]*1\M} 1 { target { rv32 } } } } */
#if __riscv_xlen == 64
SRU_REG_IMM(int64_t, 2)
/* { dg-final { scan-assembler-times {\mth.surd\t[^\n\r]*2\M} 1 { target { rv64 } } } } */
#endif

/* { dg-final { scan-assembler-not {\mslli\M} } } */
