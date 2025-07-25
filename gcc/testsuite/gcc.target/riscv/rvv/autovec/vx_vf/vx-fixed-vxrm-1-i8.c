/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -mrvv-vector-bits=zvl --param=gpr2vr-cost=0 " } */

#define VL        16

#include "vx-fixed-vxrm.h"

#define VT        vint8m1_t
#define T         int8_t
#define ELEM_SIZE 8
#define SUFFIX    i8
#define FUNC      __riscv_vaadd_vv_i8m1

DEF_FIXED_BINARY_VX_WRAP(VT, T, ELEM_SIZE, SUFFIX, __RISCV_VXRM_RNU, FUNC)
DEF_FIXED_BINARY_VX_WRAP(VT, T, ELEM_SIZE, SUFFIX, __RISCV_VXRM_RNE, FUNC)
DEF_FIXED_BINARY_VX_WRAP(VT, T, ELEM_SIZE, SUFFIX, __RISCV_VXRM_RDN, FUNC)
DEF_FIXED_BINARY_VX_WRAP(VT, T, ELEM_SIZE, SUFFIX, __RISCV_VXRM_ROD, FUNC)

/* { dg-final { scan-assembler-times {csrwi\s+vxrm,0} 1 } } */
/* { dg-final { scan-assembler-times {csrwi\s+vxrm,1} 1 } } */
/* { dg-final { scan-assembler-times {csrwi\s+vxrm,2} 1 } } */
/* { dg-final { scan-assembler-times {csrwi\s+vxrm,3} 1 } } */
/* { dg-final { scan-assembler-times {vaadd.vx} 4 } } */
