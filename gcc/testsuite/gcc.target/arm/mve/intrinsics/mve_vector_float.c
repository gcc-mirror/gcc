/* { dg-require-effective-target arm_v8_1m_mve_fp_ok } */
/* { dg-skip-if "Incompatible float ABI" { *-*-* } { "-mfloat-abi=soft" } {""} } */
/* { dg-additional-options "-march=armv8.1-m.main+mve.fp -mfloat-abi=hard -mthumb --save-temps" } */

#include "arm_mve.h"

float32x4_t
foo32 (float32x4_t value)
{
  float32x4_t b = value;
  return b;
}

/* { dg-final { scan-assembler "vmov\\tq\[0-7\], q\[0-7\]"  }  } */
/* { dg-final { scan-assembler "vstrb.*" }  } */
/* { dg-final { scan-assembler "vldmia.*" }  } */

float16x8_t
foo16 (float16x8_t value)
{
  float16x8_t b = value;
  return b;
}

/* { dg-final { scan-assembler "vmov\\tq\[0-7\], q\[0-7\]"  }  } */
/* { dg-final { scan-assembler "vstrb.*" }  } */
/* { dg-final { scan-assembler "vldmia.*" }  } */
