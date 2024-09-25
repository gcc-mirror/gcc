/* { dg-do run { target { ! avr_tiny } } } */
/* { dg-additional-options "-Os" } */

const long double vals[] =
  {
    0.0625L, -0.125L, 0.25L, -0.5L,
    1.0L,
    -2.0L, 4.0L, -8.0L, 16.0L
  };

#define ARRAY_SIZE(X) ((int) (sizeof(X) / sizeof(*X)))

__attribute__((noipa))
void test1 (long double x)
{
  int i;

  for (i = 0; i < ARRAY_SIZE (vals); ++i)
    {
      long double val0 = vals[i];
      long double val1 = __builtin_powil (x, i - 4);
      __asm ("" : "+r" (val0));

      if (val0 != val1)
	__builtin_exit (__LINE__);
    }
}

int main (void)
{
  test1 (-2.0L);
  return 0;
}
