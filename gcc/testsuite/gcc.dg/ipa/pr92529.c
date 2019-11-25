/* PR ipa/92529 */
/* { dg-options "-O2 -fdump-ipa-icf-optimized"  } */

int
foo(volatile int a)
{
  return (char)a;
}

int
bar(volatile int a)
{
  return (short)a;
}

#pragma GCC optimize ("-O0")

int main(int argc, char **argv)
{
  int r = bar(1000);
  __builtin_printf ("global: %d\n", r);
  if (r != 1000)
    __builtin_abort ();

  return 0;
}

/* { dg-final { scan-ipa-dump "Equal symbols: 0" "icf"  } } */
