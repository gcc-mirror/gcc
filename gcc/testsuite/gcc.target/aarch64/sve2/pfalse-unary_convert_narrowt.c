/* { dg-do compile } */
/* { dg-require-effective-target elf } */
/* { dg-options "-O2" } */

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

T (cvtnt, svfloat16_t, f16, svfloat32_t, f32)
T (cvtnt, svfloat32_t, f32, svfloat64_t, f64)
T (cvtxnt, svfloat32_t, f32, svfloat64_t, f64)

/* { dg-final { scan-assembler-times {\t.cfi_startproc\n\tret\n} 3 } } */
/* { dg-final { scan-assembler-times {\t.cfi_startproc\n\tmovi?\t[vdz]([0-9]+)\.?(?: [0-9]*[bhsd])?, #?0\n\tret\n} 3 } } */
/* { dg-final { scan-assembler-times {\t.cfi_startproc\n} 6 } } */
