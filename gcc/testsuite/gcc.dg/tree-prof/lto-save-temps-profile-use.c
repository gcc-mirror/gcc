/* { dg-require-effective-target lto } */
/* { dg-options "-O2 -flto=2 -flto-partition=one -save-temps" } */

static int __attribute__ ((noinline, noipa))
identity (int value)
{
  return value;
}

static int (*volatile indirect_call) (int) = identity;

int
main (void)
{
  int result = 0;

#ifdef FOR_AUTOFDO_TESTING
  const int iterations = 1000000;
#else
  const int iterations = 1;
#endif

  for (int i = 0; i < iterations; ++i)
    result |= indirect_call (0);

  return result;
}
