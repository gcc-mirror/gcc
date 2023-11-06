/* { dg-do compile } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-O1" "-Og" } } */
/* { dg-options "-march=rv64gc_xtheadbb_xtheadmemidx_xtheadfmemidx -mabi=lp64d" { target { rv64 } } } */
/* { dg-options "-march=rv32imafc_xtheadbb_xtheadmemidx_xtheadfmemidx -mabi=ilp32f" { target { rv32 } } } */

#include "xtheadmemidx-helpers.h"

LR_REG_IMM_UPD(float, 0)
#if __riscv_xlen == 64
LR_REG_IMM_UPD(double, 2)
#endif

SR_REG_IMM_UPD(float, 1)
#if __riscv_xlen == 64
SR_REG_IMM_UPD(double, 3)
#endif

/* If the shifted value is used later, we cannot eliminate it.  */
/* { dg-final { scan-assembler-times {\mslli\M} 1 { target { rv32 } } } } */
/* { dg-final { scan-assembler-times {\mslli\M} 3 { target { rv64 } } } } */
