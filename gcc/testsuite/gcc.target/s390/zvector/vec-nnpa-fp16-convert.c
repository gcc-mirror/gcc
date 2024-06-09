/* { dg-do run } */
/* { dg-require-effective-target s390_nnpa } */
/* { dg-options "-O3 -mzarch -march=arch14 -mzvector --save-temps" } */

#include <vecintrin.h>

int
main ()
{
  vector float fp1 = (vector float){ 1.0f, 2.0f, 3.0f, 4.0f };
  vector float fp2 = (vector float){ 5.0f, 6.0f, 7.0f, 8.0f };

  vector unsigned short int tmp1 = vec_round_from_fp32 (fp1, fp2, 0);

  vector unsigned short int tmp2 = vec_convert_to_fp16 (tmp1, 0);
  vector unsigned short int tmp3 = vec_convert_from_fp16 (tmp2, 0);

  vector float fp1_ret = vec_extend_to_fp32_hi (tmp3, 0);
  vector float fp2_ret = vec_extend_to_fp32_lo (tmp3, 0);

  if (vec_any_ne (fp1, fp1_ret))
    __builtin_abort ();

  if (vec_any_ne (fp2, fp2_ret))
    __builtin_abort ();

  return 0;
}

/* { dg-final { scan-assembler-times "vcrnf\t" 1 } } */
/* { dg-final { scan-assembler-times "vcfn\t" 1 } } */
/* { dg-final { scan-assembler-times "vcnf\t" 1 } } */
/* { dg-final { scan-assembler-times "vclfnh\t" 1 } } */
/* { dg-final { scan-assembler-times "vclfnl\t" 1 } } */
