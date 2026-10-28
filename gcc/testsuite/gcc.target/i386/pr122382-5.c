/* PR target/122382 */
/* { dg-do run } */
/* { dg-options "-O3 -mavx512f -mno-f16c -mno-avx512vl -mno-avx512fp16 -mprefer-vector-width=512" } */
/* { dg-require-effective-target avx512f } */

/* Only loops of at least 32 elements are loop-vectorized here, through the
   V16SF <-> V32HF pack/unpack expanders.  Shorter loops are left to SLP or
   stay scalar, calling __truncsfhf2 and __extendhfsf2.  */

#include "avx512f-check.h"
#include "pr122382.h"

void
avx512f_test (void)
{
  run_tests ();
}
