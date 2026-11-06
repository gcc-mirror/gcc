/* PR debug/128392 */
/* { dg-do run { target { i?86-*-* x86_64-*-* } } } */
/* { dg-options "-g" } */
/* { dg-require-effective-target bitint } */

volatile int sink;

__attribute__((noipa)) int
signed7 (_BitInt(7) a, _BitInt(7) b)
{
  _BitInt(7) quotient = a / b;
  _BitInt(7) remainder = a % b;
  sink += a + b; /* { dg-final { gdb-test . "(int) remainder" "1" } } */
  return quotient + remainder;
}

__attribute__((noipa)) int
signed8 (_BitInt(8) a, _BitInt(8) b)
{
  _BitInt(8) quotient = a / b;
  _BitInt(8) remainder = a % b;
  sink += a + b; /* { dg-final { gdb-test . "(int) remainder" "1" } } */
  return quotient + remainder;
}

__attribute__((noipa)) int
unsigned8 (unsigned _BitInt(8) a, unsigned _BitInt(8) b)
{
  unsigned _BitInt(8) quotient = a / b;
  unsigned _BitInt(8) remainder = a % b;
  sink += a + b; /* { dg-final { gdb-test . "(int) remainder" "3" } } */
  return quotient + remainder;
}

int
main (void)
{
  if (signed7 (55, 3) != 19
      || signed8 (55, 3) != 19
      || unsigned8 (201, 9) != 25)
    __builtin_abort ();
  return 0;
}
