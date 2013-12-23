
/* { dg-do compile } */
/* { dg-options "-march=armv8-a+crypto" } */

#include "arm_neon.h"

uint32x4_t
test_vsha256hq_u32 (uint32x4_t hash_abcd, uint32x4_t hash_efgh, uint32x4_t wk)
{
  return vsha256hq_u32 (hash_abcd, hash_efgh, wk);
}

/* { dg-final { scan-assembler-times "sha256h\\tq" 1 } } */

uint32x4_t
test_vsha256h2q_u32 (uint32x4_t hash_efgh, uint32x4_t hash_abcd, uint32x4_t wk)
{
  return vsha256h2q_u32 (hash_efgh, hash_abcd, wk);
}

/* { dg-final { scan-assembler-times "sha256h2\\tq" 1 } } */

uint32x4_t
test_vsha256su0q_u32 (uint32x4_t w0_3, uint32x4_t w4_7)
{
  return vsha256su0q_u32 (w0_3, w4_7);
}

/* { dg-final { scan-assembler-times "sha256su0\\tv" 1 } } */

uint32x4_t
test_vsha256su1q_u32 (uint32x4_t tw0_3, uint32x4_t w8_11, uint32x4_t w12_15)
{
  return vsha256su1q_u32 (tw0_3, w8_11, w12_15);
}

/* { dg-final { scan-assembler-times "sha256su1\\tv" 1 } } */


/* { dg-final { cleanup-saved-temps } } */
