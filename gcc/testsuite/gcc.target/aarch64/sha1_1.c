
/* { dg-do compile } */
/* { dg-options "-march=armv8-a+crypto" } */

#include "arm_neon.h"

uint32x4_t
test_vsha1cq_u32 (uint32x4_t hash_abcd, uint32_t hash_e, uint32x4_t wk)
{
  return vsha1cq_u32 (hash_abcd, hash_e, wk);
}

/* { dg-final { scan-assembler-times "sha1c\\tq" 1 } } */

uint32x4_t
test_vsha1mq_u32 (uint32x4_t hash_abcd, uint32_t hash_e, uint32x4_t wk)
{
  return vsha1mq_u32 (hash_abcd, hash_e, wk);
}

/* { dg-final { scan-assembler-times "sha1m\\tq" 1 } } */

uint32x4_t
test_vsha1pq_u32 (uint32x4_t hash_abcd, uint32_t hash_e, uint32x4_t wk)
{
  return vsha1pq_u32 (hash_abcd, hash_e, wk);
}

/* { dg-final { scan-assembler-times "sha1p\\tq" 1 } } */

uint32_t
test_vsha1h_u32 (uint32_t hash_e)
{
  return vsha1h_u32 (hash_e);
}

/* { dg-final { scan-assembler-times "sha1h\\ts" 1 } } */

uint32x4_t
test_vsha1su0q_u32 (uint32x4_t w0_3, uint32x4_t w4_7, uint32x4_t w8_11)
{
  return vsha1su0q_u32 (w0_3, w4_7, w8_11);
}

/* { dg-final { scan-assembler-times "sha1su0\\tv" 1 } } */

uint32x4_t
test_vsha1su1q_u32 (uint32x4_t tw0_3, uint32x4_t w12_15)
{
  return vsha1su1q_u32 (tw0_3, w12_15);
}

/* { dg-final { scan-assembler-times "sha1su1\\tv" 1 } } */

