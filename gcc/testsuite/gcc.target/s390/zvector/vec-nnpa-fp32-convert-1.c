/* { dg-do run } */
/* { dg-require-effective-target s390_nnpa } */
/* { dg-options "-O3 -mzarch -march=arch14 -mzvector --save-temps" } */

#include <vecintrin.h>

int
main ()
{
  vector float fp1 = (vector float){ 1.0f, 2.0f, 3.0f, 4.0f };
  vector float fp2 = (vector float){ 5.0f, 6.0f, 7.0f, 8.0f };
  vector short int conv = vec_round_from_fp32 (fp1, fp2, 0);
  vector float fp1_ret = vec_extend_to_fp32_hi (conv, 0);
  vector float fp2_ret = vec_extend_to_fp32_lo (conv, 0);

  if (vec_any_ne (fp1, fp1_ret))
    __builtin_abort ();

  if (vec_any_ne (fp2, fp2_ret))
    __builtin_abort ();

  return 0;
}

/* { dg-final { scan-assembler-times "vcrnf\t" 1 } } */
/* { dg-final { scan-assembler-times "vclfnh\t" 1 } } */
/* { dg-final { scan-assembler-times "vclfnl\t" 1 } } */
