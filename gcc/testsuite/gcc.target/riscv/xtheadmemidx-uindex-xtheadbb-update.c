/* { dg-do compile } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-O1" "-Og" } } */
/* { dg-options "-march=rv64gc_xtheadbb_xtheadmemidx" { target { rv64 } } } */
/* { dg-options "-march=rv32gc_xtheadbb_xtheadmemidx" { target { rv32 } } } */

#include "xtheadmemidx-helpers.h"

LRU_REG_IMM_UPD(int8_t, 0)
LRU_REG_IMM_UPD(uint8_t, 1)
LRU_REG_IMM_UPD(int16_t, 2)
LRU_REG_IMM_UPD(uint16_t, 3)
LRU_REG_IMM_UPD(int32_t, 0)
#if __riscv_xlen == 64
LRU_REG_IMM_UPD(uint32_t, 1)
LRU_REG_IMM_UPD(int64_t, 2)
#endif

SRU_REG_IMM_UPD(int8_t, 3)
SRU_REG_IMM_UPD(int16_t, 0)
SRU_REG_IMM_UPD(int32_t, 1)
#if __riscv_xlen == 64
SRU_REG_IMM_UPD(int64_t, 2)
#endif

/* If the shifted value is used later, we cannot eliminate it.  */
/* { dg-final { scan-assembler-times {\mslli\M} 5 { target { rv32 } } } } */
/* { dg-final { scan-assembler-times {\mslli\M} 8 { target { rv64 } } } } */
