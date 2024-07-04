/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64 -O3" } */

#include "riscv_vector.h"

#define CMP_FLOAT_VF_1(ID, S, OP, IMM)                                         \
  vbool##S##_t test_float_1_##ID##_##S (vfloat##S##m1_t op1, size_t vl)        \
  {                                                                            \
    return __riscv_vmf##OP##_vf_f##S##m1_b##S (op1, IMM, vl);                  \
  }

CMP_FLOAT_VF_1 (0, 32, eq, 0.0)
CMP_FLOAT_VF_1 (1, 32, eq, 1.0)
CMP_FLOAT_VF_1 (2, 32, eq, __builtin_nanf ("123"))
CMP_FLOAT_VF_1 (3, 32, ne, 0.0)
CMP_FLOAT_VF_1 (4, 32, ne, 1.0)
CMP_FLOAT_VF_1 (5, 32, ne, __builtin_nanf ("123"))

CMP_FLOAT_VF_1 (0, 64, eq, 0.0)
CMP_FLOAT_VF_1 (1, 64, eq, 1.0)
CMP_FLOAT_VF_1 (2, 64, eq, __builtin_nan ("123"))
CMP_FLOAT_VF_1 (3, 64, ne, 0.0)
CMP_FLOAT_VF_1 (4, 64, ne, 1.0)
CMP_FLOAT_VF_1 (5, 64, ne, __builtin_nan ("123"))

#define CMP_FLOAT_VF_2(ID, S, OP, IMM)                                         \
  vfloat##S##m1_t test_float_2_##ID##_##S (vfloat##S##m1_t op1,                \
					   vfloat##S##m1_t op2, size_t vl)     \
  {                                                                            \
    vfloat##S##m1_t op3 = __riscv_vfmv_s_f_f##S##m1 (IMM, vl);                 \
    vbool##S##_t mask1 = __riscv_vmf##OP##_vf_f##S##m1_b##S (op1, IMM, vl);    \
    vbool##S##_t mask2 = __riscv_vmf##OP##_vv_f##S##m1_b##S (op1, op3, vl);    \
    vbool##S##_t mask3 = __riscv_vmor (mask1, mask2, vl);                      \
    return __riscv_vmerge_vvm_f##S##m1_tu (op1, op1, op2, mask3, vl);          \
  }

CMP_FLOAT_VF_2 (0, 32, eq, 0.0)
CMP_FLOAT_VF_2 (1, 32, eq, 1.0)
CMP_FLOAT_VF_2 (2, 32, eq, __builtin_nanf ("123"))
CMP_FLOAT_VF_2 (3, 32, ne, 0.0)
CMP_FLOAT_VF_2 (4, 32, ne, 1.0)
CMP_FLOAT_VF_2 (5, 32, ne, __builtin_nanf ("123"))

CMP_FLOAT_VF_2 (0, 64, eq, 0.0)
CMP_FLOAT_VF_2 (1, 64, eq, 1.0)
CMP_FLOAT_VF_2 (2, 64, eq, __builtin_nan ("123"))
CMP_FLOAT_VF_2 (3, 64, ne, 0.0)
CMP_FLOAT_VF_2 (4, 64, ne, 1.0)
CMP_FLOAT_VF_2 (5, 64, ne, __builtin_nan ("123"))

/* { dg-final { scan-assembler-times {vmfeq\.vf} 12 } } */
/* { dg-final { scan-assembler-times {vmfne\.vf} 12 } } */
/* { dg-final { scan-assembler-times {vmfeq\.vv} 6 } } */
/* { dg-final { scan-assembler-times {vmfne\.vv} 6 } } */
