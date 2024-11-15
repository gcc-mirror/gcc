/* { dg-do compile } */
/* { dg-require-effective-target elf } */
/* { dg-options "-O2 -march=armv8.2-a+sve+bf16" } */

#include <arm_sve.h>

#define T(F, TYPE1, TY1, TYPE2, TY2)					\
  TYPE1 F##_##TY1##_##TY2##_x_f (TYPE1 even, TYPE2 op)			\
  {									\
    return sv##F##_##TY1##_##TY2##_x (even, svpfalse_b (), op);		\
  }									\
  TYPE1 F##_##TY1##_##TY2##_m_f (TYPE1 even, TYPE2 op)			\
  {									\
    return sv##F##_##TY1##_##TY2##_m (even, svpfalse_b (), op);		\
  }

T (cvtnt, svbfloat16_t, bf16, svfloat32_t, f32)

/* { dg-final { scan-assembler-times {\t.cfi_startproc\n\tret\n} 1 } } */
/* { dg-final { scan-assembler-times {\t.cfi_startproc\n\tmovi?\t[vdz]([0-9]+)\.?(?: [0-9]*[bhsd])?, #?0\n\tret\n} 1 } } */
/* { dg-final { scan-assembler-times {\t.cfi_startproc\n} 2 } } */
