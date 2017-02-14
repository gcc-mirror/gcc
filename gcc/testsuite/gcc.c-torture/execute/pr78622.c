/* PR middle-end/78622 - [7 Regression] -Wformat-overflow/-fprintf-return-value
   incorrect with overflow/wrapping
   { dg-skip-if "Requires %hhd format" { hppa*-*-hpux* } { "*" } { "" } }
   { dg-additional-options "-Wformat-overflow=2" } */

__attribute__((noinline, noclone)) int
foo (int x)
{
  if (x < 4096 + 8 || x >= 4096 + 256 + 8)
    return -1;

  char buf[5];
  int n = __builtin_snprintf (buf, sizeof buf, "%hhd", x + 1);
  __builtin_printf ("\"%hhd\" => %i\n", x + 1, n);
  return n;
}

int
main (void)
{
  if (__SCHAR_MAX__ != 127 || __CHAR_BIT__ != 8 || __SIZEOF_INT__ != 4)
    return 0;

  if (foo (4095 + 9) != 1
      || foo (4095 + 32) != 2
      || foo (4095 + 127) != 3
      || foo (4095 + 128) != 4
      || foo (4095 + 240) != 3
      || foo (4095 + 248) != 2
      || foo (4095 + 255) != 2
      || foo (4095 + 256) != 1)
    __builtin_abort ();

  return 0;
}
