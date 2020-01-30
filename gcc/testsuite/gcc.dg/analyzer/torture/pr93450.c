void
ed (int);

double
bg (void)
{
  double kl = __builtin_huge_val ();

  ed (0);

  return kl;
}

static double __attribute__((noinline))
get_hugeval (void)
{
  return __builtin_huge_val ();
}

int test_2 (int i)
{
  if (i < get_hugeval ())
    return 42;
  return 0;
}
