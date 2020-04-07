/* { dg-require-effective-target arm_v8_1m_mve_fp_ok } */
/* { dg-add-options arm_v8_1m_mve_fp } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

uint8x16_t u8;
int8x16_t s8;
uint16x8_t u16;
int16x8_t s16;
uint32x4_t u32;
int32x4_t s32;
float16x8_t f16;
float32x4_t f32;

#define MACRO_VCVT_NAME(__to1,__from1) \
  vcvtq_n_##__to1 ## _ ##__from1

#define MACRO_VCVT_NAME_P(__to1,__from1) \
  vcvtq_m_n_##__to1 ## _ ##__from1

#define VCVT_TEST(to,from,val) \
  to = MACRO_VCVT_NAME(to,from) (from, val); \
  to = MACRO_VCVT_NAME_P(to,from) (to, from, val, p);

#define VQSHRN_TEST(to,from,val) \
  to = vqshrnbq (to, from, val); \
  to = vqshrntq (to, from, val); \
  to = vqshrnbq_m (to, from, val, p); \
  to = vqshrntq_m (to, from, val, p); \

#define VQSHRUN_TEST(to,from,val) \
  to = vqshrunbq (to, from, val); \
  to = vqshruntq (to, from, val); \
  to = vqshrunbq_m (to, from, val, p); \
  to = vqshruntq_m (to, from, val, p); \

void test (mve_pred16_t p)
{
  VCVT_TEST(f16,s16,16)
  VCVT_TEST(f16,u16,16)
  VCVT_TEST(f32,s32,32)
  VCVT_TEST(f32,u32,32)
  VCVT_TEST(u16,f16,16)
  VCVT_TEST(s16,f16,16)
  VCVT_TEST(u32,f32,32)
  VCVT_TEST(s32,f32,32)
  VCVT_TEST(f16,s16,1)
  VCVT_TEST(f16,u16,1)
  VCVT_TEST(f32,s32,1)
  VCVT_TEST(f32,u32,1)
  VCVT_TEST(u16,f16,1)
  VCVT_TEST(s16,f16,1)
  VCVT_TEST(u32,f32,1)
  VCVT_TEST(s32,f32,1)
  VQSHRN_TEST(u8, u16, 8)
  VQSHRN_TEST(s8, s16, 8)
  VQSHRN_TEST(u16, u32, 16)
  VQSHRN_TEST(s16, s32, 16)
  VQSHRUN_TEST(u8, s16, 8)
  VQSHRUN_TEST(u16, s32, 16)
}
