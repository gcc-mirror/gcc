/* Test __bf16 built-in functions.  */
/* { dg-do run } */
/* { dg-options "" } */
/* { dg-add-options bfloat16 } */
/* { dg-add-options ieee } */
/* { dg-require-effective-target bfloat16_runtime } */

extern void exit (int);
extern void abort (void);

extern __bf16 test_type;
extern __typeof (__builtin_nansf16b ("")) test_type;

volatile __bf16 inf_cst = (__bf16) __builtin_inff ();
volatile __bf16 huge_val_cst = (__bf16) __builtin_huge_valf ();
volatile __bf16 nan_cst = (__bf16) __builtin_nanf ("");
volatile __bf16 nans_cst = __builtin_nansf16b ("");
volatile __bf16 neg0 = -0.0bf16, neg1 = -1.0bf16, one = 1.0;

int
main (void)
{
  volatile __bf16 r;
  if (!__builtin_isinf (inf_cst))
    abort ();
  if (!__builtin_isinf (huge_val_cst))
    abort ();
  if (inf_cst != huge_val_cst)
    abort ();
  if (!__builtin_isnan (nan_cst))
    abort ();
  if (!__builtin_isnan (nans_cst))
    abort ();
  r = __builtin_fabsf (neg1);
  if (r != 1.0bf16)
    abort ();
  r = __builtin_copysignf (one, neg0);
  if (r != neg1)
    abort ();
  r = __builtin_copysignf (inf_cst, neg1);
  if (r != -huge_val_cst)
    abort ();
  r = __builtin_copysignf (-inf_cst, one);
  if (r != huge_val_cst)
    abort ();
  exit (0);
}
