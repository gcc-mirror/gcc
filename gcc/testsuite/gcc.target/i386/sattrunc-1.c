/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-final { scan-assembler-times "sbb|cmov" 6 { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-times "sbb|cmov" 3 { target ia32 } } } */

#include <stdint.h>

#define DEF_SAT_U_TRUNC(WT, NT)			\
NT sat_u_truc_##WT##_to_##NT (WT x)		\
{						\
  _Bool overflow = x > (WT)(NT)(-1);		\
  return (NT)x | (NT)-overflow;			\
}

#ifdef __x86_64__
DEF_SAT_U_TRUNC(uint64_t, uint32_t)
DEF_SAT_U_TRUNC(uint64_t, uint16_t)
DEF_SAT_U_TRUNC(uint64_t, uint8_t)
#endif

DEF_SAT_U_TRUNC(uint32_t, uint16_t)
DEF_SAT_U_TRUNC(uint32_t, uint8_t)

DEF_SAT_U_TRUNC(uint16_t, uint8_t)
