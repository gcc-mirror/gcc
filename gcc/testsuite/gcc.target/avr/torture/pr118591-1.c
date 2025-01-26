/* { dg-do run { target { ! avr_tiny } } } */
/* { dg-additional-options "-std=c99 -mlra" } */

__attribute__((noipa))
void func2 (long long a1, long long a2, long b)
{
  static unsigned char count = 0;
  if (b != count++)
    __builtin_abort ();
}

int main (void)
{
  for (long b = 0; b < 5; ++b)
    {
      __asm ("" ::: "r5", "r9", "r24", "r20", "r16", "r12", "r30");

      func2 (0, 0, b);
    }

  return 0;
}
