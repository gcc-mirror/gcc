/* { dg-do compile } */
/* { dg-require-effective-target elf } */
/* { dg-options "-O2 -march=armv9.2-a+sve+sme -funwind-tables" } */

#include "../pfalse-unary_0.h"

ALL_SIGNED (qabs, MXZ)
ALL_SIGNED (qneg, MXZ)
S_UNSIGNED (recpe, MXZ)
S_UNSIGNED (rsqrte, MXZ)

#undef M
#define M(F, RTY, TY)						\
  __arm_streaming						\
  RTY F##_f (RTY inactive, TY op)				\
  {								\
    return sv##F (inactive, svpfalse_b (), op);			\
  }

#undef XZI
#define XZI(F, RTY, TY)						\
  __arm_streaming						\
  RTY F##_f (TY op)						\
  {								\
    return sv##F (svpfalse_b (), op);				\
  }

ALL_DATA (revd, MXZ)
 
/* { dg-final { scan-assembler-times {\t.cfi_startproc\n\tret\n} 22 } } */
/* { dg-final { scan-assembler-times {\t.cfi_startproc\n\tmovi?\t[vdz]([0-9]+)\.?(?:[0-9]*[bhsd])?, #?0\n\tret\n} 44 } } */
/* { dg-final { scan-assembler-times {\t.cfi_startproc\n} 66 } } */
