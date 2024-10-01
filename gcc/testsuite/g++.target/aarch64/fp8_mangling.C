/* Test that mfloat8_t mangles differently from uint8_t  */
/* { dg-options "-O1 -march=armv9.4-a+fp8" } */

int
foo (__mfp8)
{
  return 1;
}

int
foo (unsigned char)
{
  return 2;
}

int
bar (__mfp8 x)
{
  return foo (x);
}
/* { dg-final { scan-assembler-times "\n_Z3fooh:\n" 1 } } */
/* { dg-final { scan-assembler-times "\n_Z3foou6__mfp8:\n" 1 } } */

constexpr __mfp8 cfp8{};

constexpr int
fooc (unsigned char)
{
  return 3;
}

constexpr int
fooc (__mfp8)
{
  return 4;
}

constexpr int
barc (__mfp8 x)
{
  return fooc (x);
}

static_assert (barc (cfp8) == 4, "constexpr selects incorrect overload");
