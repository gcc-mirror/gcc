/* { dg-require-effective-target arm_hard_ok } */
/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-mfloat-abi=hard" } */
/* { dg-skip-if "Incompatible float ABI" { *-*-* } { "-mfloat-abi=soft" } {""} } */

#include "arm_mve.h"

uint8x16_t
foo8 (uint8x16_t value)
{
  uint8x16_t b = {1, 2, 3, 4};
  return b;
}

/* { dg-final { scan-assembler "vmov\\tq\[0-7\], q\[0-7\]"  }  } */
/* { dg-final { scan-assembler "vstrb.*" }  } */
/* { dg-final { scan-assembler "vldr.64.*" }  } */

uint16x8_t
foo16 (uint16x8_t value)
{
  uint16x8_t b = {1, 2, 3};
  return b;
}

/* { dg-final { scan-assembler "vmov\\tq\[0-7\], q\[0-7\]"  }  } */
/* { dg-final { scan-assembler "vstrb.*" }  } */
/* { dg-final { scan-assembler "vldr.64.*" }  } */

uint32x4_t
foo32 (uint32x4_t value)
{
  uint32x4_t b = {1, 2};
  return b;
}

/* { dg-final { scan-assembler "vmov\\tq\[0-7\], q\[0-7\]"  }  } */
/* { dg-final { scan-assembler "vstrb.*" }  } */
/* { dg-final { scan-assembler "vldr.64.*" }  } */

uint64x2_t
foo64 (uint64x2_t value)
{
  uint64x2_t b = {1};
  return b;
}

/* { dg-final { scan-assembler "vmov\\tq\[0-7\], q\[0-7\]"  }  } */
/* { dg-final { scan-assembler "vstrb.*" }  } */
/* { dg-final { scan-assembler "vldr.64.*" }  } */
