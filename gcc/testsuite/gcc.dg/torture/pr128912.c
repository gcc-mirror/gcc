/* PR middle-end/128460 */
/* PR middle-end/128912 */
/* { dg-do run } */
/* { dg-require-effective-target float16_runtime } */
/* { dg-require-effective-target bfloat16_runtime } */
/* { dg-add-options float16 } */
/* { dg-add-options bfloat16 } */

/* _Float16 and __bf16 have the same width but cannot represent each
   other's values.  */

__attribute__ ((noipa)) _Float16
h_b_h (_Float16 x)
{
  return (_Float16) (__bf16) x;
}

__attribute__ ((noipa)) float
h_b_f (_Float16 x)
{
  return (float) (__bf16) x;
}

__attribute__ ((noipa)) __bf16
b_h_b (__bf16 x)
{
  return (__bf16) (_Float16) x;
}

__attribute__ ((noipa)) float
b_h_f (__bf16 x)
{
  return (float) (_Float16) x;
}

/* 65504.0f rounds to 65536.0 in __bf16, which overflows _Float16.  */

__attribute__ ((noipa)) double
f_d (float a)
{
  return (double) (_Float16) (__bf16) a;
}

__attribute__ ((noipa)) float
f_f (float a)
{
  return (float) (_Float16) (__bf16) a;
}

int
main (void)
{
  volatile _Float16 h = 1.0009765625f16;
  volatile __bf16 b = 70016.0bf16;
  volatile _Float16 hr;
  volatile __bf16 br;

  hr = h;
  br = hr;
  hr = br;
  if (h_b_h (h) != hr)
    __builtin_abort ();
  br = h;
  if (h_b_f (h) != (float) br)
    __builtin_abort ();

  hr = b;
  br = hr;
  if (b_h_b (b) != br)
    __builtin_abort ();
  hr = b;
  if (b_h_f (b) != (float) hr)
    __builtin_abort ();

  if (f_d (65504.0f) != __builtin_inf ())
    __builtin_abort ();
  if (f_f (65504.0f) != __builtin_inff ())
    __builtin_abort ();
  return 0;
}
