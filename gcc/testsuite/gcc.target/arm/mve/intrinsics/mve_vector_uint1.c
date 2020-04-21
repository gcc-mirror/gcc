/* { dg-require-effective-target arm_hard_ok } */
/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-mfloat-abi=hard" } */
/* { dg-skip-if "Incompatible float ABI" { *-*-* } { "-mfloat-abi=soft" } {""} } */

#include "arm_mve.h"

uint8x16_t value1;
uint16x8_t value2;
uint32x4_t value3;
uint64x2_t value4;

uint8x16_t
foo8 ()
{
  uint8x16_t b = value1;
  return b;
}

/* { dg-final { scan-assembler "vmov\\tq\[0-7\], q\[0-7\]"  }  } */
/* { dg-final { scan-assembler "vstrb.*" }  } */
/* { dg-final { scan-assembler "vldrb.8*" }  } */

uint16x8_t
foo16 ()
{
  uint16x8_t b = value2;
  return b;
}

/* { dg-final { scan-assembler "vmov\\tq\[0-7\], q\[0-7\]"  }  } */
/* { dg-final { scan-assembler "vstrb.*" }  } */
/* { dg-final { scan-assembler "vldrb.8*" }  } */

uint32x4_t
foo32 ()
{
  uint32x4_t b = value3;
  return b;
}

/* { dg-final { scan-assembler "vmov\\tq\[0-7\], q\[0-7\]"  }  } */
/* { dg-final { scan-assembler "vstrb.*" }  } */
/* { dg-final { scan-assembler "vldrb.8*" }  } */

uint64x2_t
foo64 ()
{
  uint64x2_t b = value4;
  return b;
}

/* { dg-final { scan-assembler "vmov\\tq\[0-7\], q\[0-7\]"  }  } */
/* { dg-final { scan-assembler "vstrb.*" }  } */
/* { dg-final { scan-assembler "vldrb.8*" }  } */
