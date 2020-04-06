/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-skip-if "Incompatible float ABI" { *-*-* } { "-mfloat-abi=soft" } {""} } */
/* { dg-additional-options "-march=armv8.1-m.main+mve -mfpu=auto -mfloat-abi=hard -mthumb --save-temps" } */

#include "arm_mve.h"

int8x16_t
foo8 (int8x16_t value)
{
  int8x16_t b = value;
  return b;
}

/* { dg-final { scan-assembler "vmov\\tq\[0-7\], q\[0-7\]"  }  } */
/* { dg-final { scan-assembler "vstrb.*" }  } */
/* { dg-final { scan-assembler "vldrb.8*" }  } */

int16x8_t
foo16 (int16x8_t value)
{
  int16x8_t b = value;
  return b;
}

/* { dg-final { scan-assembler "vmov\\tq\[0-7\], q\[0-7\]"  }  } */
/* { dg-final { scan-assembler "vstrb.*" }  } */
/* { dg-final { scan-assembler "vldrb.8*" }  } */

int32x4_t
foo32 (int32x4_t value)
{
  int32x4_t b = value;
  return b;
}

/* { dg-final { scan-assembler "vmov\\tq\[0-7\], q\[0-7\]"  }  } */
/* { dg-final { scan-assembler "vstrb.*" }  } */
/* { dg-final { scan-assembler "vldrb.8*" }  } */

int64x2_t
foo64 (int64x2_t value)
{
  int64x2_t b = value;
  return b;
}

/* { dg-final { scan-assembler "vmov\\tq\[0-7\], q\[0-7\]"  }  } */
/* { dg-final { scan-assembler "vstrb.*" }  } */
/* { dg-final { scan-assembler "vldrb.8*" }  } */
