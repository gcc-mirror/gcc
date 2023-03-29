/* { dg-do compile { target { x86_64-*-* && lp64 } } } */
/* { dg-additional-options "-mrdrnd" } */

unsigned short
hardware_rand16 (void)
{
  unsigned short x;
  while (! __builtin_ia32_rdrand16_step (&x))
    continue;
  return x; /* { dg-bogus "uninit" } */
}

unsigned int
hardware_rand32 (void)
{
  unsigned int x;
  while (! __builtin_ia32_rdrand32_step (&x))
    continue;
  return x; /* { dg-bogus "uninit" } */
}

unsigned long long
hardware_rand64 (void)
{
  unsigned long long int x;
  while (! __builtin_ia32_rdrand64_step (&x))
    continue;
  return x; /* { dg-bogus "uninit" } */
}
