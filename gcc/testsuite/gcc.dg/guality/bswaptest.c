/* { dg-do run { target { { i?86-*-* x86_64-*-* } && lp64 } } } */
/* { dg-options "-g" } */

volatile int vv;

__attribute__((noclone, noinline)) long
foo (long x)
{
  long f = __builtin_bswap64 (x);
  long g = f;
  asm volatile ("" : "+r" (f));
  vv++;		/* { dg-final { gdb-test 12 "g" "f" } } */
  return f;
}

__attribute__((noclone, noinline)) int
bar (int x)
{
  int f = __builtin_bswap32 (x);
  int g = f;
  asm volatile ("" : "+r" (f));
  vv++;		/* { dg-final { gdb-test 22 "g" "f" } } */
  return f;
}

int
main ()
{
  foo (0x123456789abcde0fUL);
  bar (0x12345678);
  return 0;
}
