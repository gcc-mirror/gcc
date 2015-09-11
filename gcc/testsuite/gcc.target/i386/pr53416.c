/* PR target/53416 */
/* { dg-do compile } */
/* { dg-options "-O2 -mrdrnd" } */

int test (void)
{
  unsigned int number = 0;
  int result0, result1, result2, result3;

  result0 = __builtin_ia32_rdrand32_step (&number);
  result1 = __builtin_ia32_rdrand32_step (&number);
  result2 = __builtin_ia32_rdrand32_step (&number);
  result3 = __builtin_ia32_rdrand32_step (&number);

  return result0 + result1 +result2 + result3;
}

/* { dg-final { scan-assembler-times "rdrand" 4 } } */
