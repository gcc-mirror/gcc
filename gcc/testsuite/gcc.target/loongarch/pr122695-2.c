/* { dg-do run } */
/* { dg-options "-O2 -mlasx" } */
/* { dg-require-effective-target loongarch_asx_hw } */

[[gnu::vector_size(32)]] short a, b, c;

[[gnu::noipa]] void
test (void)
{
  a = __builtin_shuffle(a, b, c) + c;
}

int
main (void)
{
  a = (typeof (a)){} + 1;
  b = (typeof (b)){} + 2;
  c = (typeof (c)){} + 128;
  test ();
  if (a[0] != 129)
    __builtin_trap ();
}
