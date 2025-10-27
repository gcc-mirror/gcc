/* { dg-do compile } */
/* { dg-require-effective-target elf } */
/* { dg-options "-O2 -funwind-tables" } */

#include <arm_sve.h>

#define XZ(F, TYPE1, TY1, TYPE2, TY2, P)				\
  TYPE1 F##_##TY1##_##TY2##_##P##_f (TYPE2 op)				\
  {									\
    return sv##F##_##TY1##_##TY2##_##P (svpfalse_b (), op);		\
  }									\

#define M(F, TYPE1, TY1, TYPE2, TY2)					\
  TYPE1 F##_##TY1##_##TY2##_m_f (TYPE1 inactive, TYPE2 op)		\
  {									\
    return sv##F##_##TY1##_##TY2##_m (inactive, svpfalse_b (), op);	\
  }

M (cvtx, svfloat32_t, f32, svfloat64_t, f64)
XZ (cvtx, svfloat32_t, f32, svfloat64_t, f64, x)
XZ (cvtx, svfloat32_t, f32, svfloat64_t, f64, z)
M (cvtlt, svfloat32_t, f32, svfloat16_t, f16)
XZ (cvtlt, svfloat32_t, f32, svfloat16_t, f16, x)
M (cvtlt, svfloat64_t, f64, svfloat32_t, f32)
XZ (cvtlt, svfloat64_t, f64, svfloat32_t, f32, x)



/* { dg-final { scan-assembler-times {\t.cfi_startproc\n\tret\n}  3 } } */
/* { dg-final { scan-assembler-times {\t.cfi_startproc\n\tmovi?\t[vdz]([0-9]+)\.?(?:[0-9]*[bhsd])?, #?0\n\tret\n} 4 } } */
/* { dg-final { scan-assembler-times {\t.cfi_startproc\n} 7 } } */
