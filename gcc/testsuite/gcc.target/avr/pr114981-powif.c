/* { dg-do run { target { ! avr_tiny } } } */
/* { dg-additional-options "-Os" } */

const float vals[] =
  {
    0.0625f, -0.125f, 0.25f, -0.5f,
    1.0f,
    -2.0f, 4.0f, -8.0f, 16.0f
  };

#define ARRAY_SIZE(X) ((int) (sizeof(X) / sizeof(*X)))

__attribute__((noinline,noclone))
void test1 (float x)
{
  int i;

  for (i = 0; i < ARRAY_SIZE (vals); ++i)
    {
      float val0 = vals[i];
      float val1 = __builtin_powif (x, i - 4);
      __asm ("" : "+r" (val0));

      if (val0 != val1)
	__builtin_exit (__LINE__);
    }
}

int main (void)
{
  test1 (-2.0f);
  return 0;
}
