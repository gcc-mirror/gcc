/* { dg-do compile } */
/* { dg-options "-O2 -mcpu=thunderx+nofp -march=armv8-a" } */

#include "arm_neon.h"

/* Unless we do something about re-laying out the SIMD builtin types
   this testcase ICEs during expansion of the crypto builtin.  */

__attribute__ ((target ("cpu=cortex-a57+sha2")))
uint32x4_t
test_vsha1cq_u32 (uint32x4_t hash_abcd, uint32_t hash_e, uint32x4_t wk)
{
  return vsha1cq_u32 (hash_abcd, hash_e, wk);
}

/* This one should be compiled for thunderx with no fp.  */
int
foo (int a)
{
  return a + 5;
}
