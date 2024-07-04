/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64 -O3" } */

#include "riscv_vector.h"

#define CMP_VF_1(ID, S, OP)                                                    \
  vbool##S##_t test_1_##ID##_##S##_##OP (vint##S##m1_t op1, int##S##_t op2,    \
					 size_t vl)                            \
  {                                                                            \
    return __riscv_vms##OP##_vx_i##S##m1_b##S (op1, op2, vl);                  \
  }

CMP_VF_1 (0, 8, eq)
CMP_VF_1 (1, 16, eq)
CMP_VF_1 (2, 32, eq)
CMP_VF_1 (3, 64, eq)

CMP_VF_1 (0, 8, ne)
CMP_VF_1 (1, 16, ne)
CMP_VF_1 (2, 32, ne)
CMP_VF_1 (3, 64, ne)

#define CMP_VF_2(ID, S, OP, IMM)                                               \
  vbool##S##_t test_2_##ID##_##S##_##OP (vint##S##m1_t op1, size_t vl)         \
  {                                                                            \
    return __riscv_vms##OP##_vx_i##S##m1_b##S (op1, IMM, vl);                  \
  }

CMP_VF_2 (0, 8, eq, -16)
CMP_VF_2 (1, 8, eq, 15)
CMP_VF_2 (2, 8, eq, -17)
CMP_VF_2 (3, 8, eq, 16)
CMP_VF_2 (4, 16, eq, -16)
CMP_VF_2 (5, 16, eq, 15)
CMP_VF_2 (6, 16, eq, -17)
CMP_VF_2 (7, 16, eq, 16)
CMP_VF_2 (8, 32, eq, -16)
CMP_VF_2 (9, 32, eq, 15)
CMP_VF_2 (10, 32, eq, -17)
CMP_VF_2 (11, 32, eq, 16)
CMP_VF_2 (12, 64, eq, -16)
CMP_VF_2 (13, 64, eq, 15)
CMP_VF_2 (14, 64, eq, -17)
CMP_VF_2 (15, 64, eq, 16)

CMP_VF_2 (0, 8, ne, -16)
CMP_VF_2 (1, 8, ne, 15)
CMP_VF_2 (2, 8, ne, -17)
CMP_VF_2 (3, 8, ne, 16)
CMP_VF_2 (4, 16, ne, -16)
CMP_VF_2 (5, 16, ne, 15)
CMP_VF_2 (6, 16, ne, -17)
CMP_VF_2 (7, 16, ne, 16)
CMP_VF_2 (8, 32, ne, -16)
CMP_VF_2 (9, 32, ne, 15)
CMP_VF_2 (10, 32, ne, -17)
CMP_VF_2 (11, 32, ne, 16)
CMP_VF_2 (12, 64, ne, -16)
CMP_VF_2 (13, 64, ne, 15)
CMP_VF_2 (14, 64, ne, -17)
CMP_VF_2 (15, 64, ne, 16)

/* { dg-final { scan-assembler-times {vmseq\.vx} 12 } } */
/* { dg-final { scan-assembler-times {vmsne\.vx} 12 } } */
/* { dg-final { scan-assembler-times {vmseq\.vi} 8 } } */
/* { dg-final { scan-assembler-times {vmsne\.vi} 8 } } */
