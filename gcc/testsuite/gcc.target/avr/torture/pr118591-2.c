/* Test case failed on avrtiny.  */
/* { dg-do run } */
/* { dg-additional-options "-std=c99 -mlra" } */

__attribute__((noipa))
void func2 (long a, long b)
{
  static unsigned char count = 0;
  if (b != count++)
    __builtin_abort ();
}

int main (void)
{
  for (long b = 0; b < 5; ++b)
    {
      func2 (0, b);
    }

  return 0;
}
