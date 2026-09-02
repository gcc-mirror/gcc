#ifndef ACE_HELPER_H_INCLUDED
#define ACE_HELPER_H_INCLUDED
#define ACE
#define AVX512FP16
#define AVX512BF16
#include "avx512f-helper.h"
#include "fp8-helper.h"

float convert_e8m0_to_fp32 (unsigned char x)
{   
  unsigned int tmp = ((unsigned int) x) << 23;
  Floatuint32Union ux = { .u = tmp };
  if (x == 0xff)
    ux.u |= 0x1;
  return ux.f;
}

#endif
