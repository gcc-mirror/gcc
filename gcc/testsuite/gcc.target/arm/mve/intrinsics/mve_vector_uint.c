/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-skip-if "Incompatible float ABI" { *-*-* } { "-mfloat-abi=soft" } {""} } */
/* { dg-additional-options "-march=armv8.1-m.main+mve -mfpu=auto -mfloat-abi=hard -mthumb --save-temps" } */

#include "arm_mve.h"

uint8x16_t
foo8 (uint8x16_t value)
{
  uint8x16_t b = value;
  return b;
}

/* { dg-final { scan-assembler "vmov\\tq\[0-7\], q\[0-7\]"  }  } */
/* { dg-final { scan-assembler "vstrb.*" }  } */
/* { dg-final { scan-assembler "vldrb.8*" }  } */

uint16x8_t
foo16 (uint16x8_t value)
{
  uint16x8_t b = value;
  return b;
}

/* { dg-final { scan-assembler "vmov\\tq\[0-7\], q\[0-7\]"  }  } */
/* { dg-final { scan-assembler "vstrb.*" }  } */
/* { dg-final { scan-assembler "vldrb.8*" }  } */

uint32x4_t
foo32 (uint32x4_t value)
{
  uint32x4_t b = value;
  return b;
}

/* { dg-final { scan-assembler "vmov\\tq\[0-7\], q\[0-7\]"  }  } */
/* { dg-final { scan-assembler "vstrb.*" }  } */
/* { dg-final { scan-assembler "vldrb.8*" }  } */

uint64x2_t
foo64 (uint64x2_t value)
{
  uint64x2_t b = value;
  return b;
}

/* { dg-final { scan-assembler "vmov\\tq\[0-7\], q\[0-7\]"  }  } */
/* { dg-final { scan-assembler "vstrb.*" }  } */
/* { dg-final { scan-assembler "vldrb.8*" }  } */
