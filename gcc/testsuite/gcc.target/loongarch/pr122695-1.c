/* { dg-do run } */
/* { dg-options "-O2 -mlasx" } */
/* { dg-require-effective-target loongarch_asx_hw } */

[[gnu::vector_size (32)]] char a, b, c;

[[gnu::noipa]] void
test (void)
{
  a = __builtin_shuffle (a, b, c);
}

int
main (void)
{
  a = (typeof (a)){} + 5;
  b = (typeof (a)){} + 6;
  c = (typeof (a)){} + 64;
  test ();
  if (a[0] != 5)
    __builtin_trap ();
}
