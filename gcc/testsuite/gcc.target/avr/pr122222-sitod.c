/* { dg-do run { target { ! avr_tiny } } } */
/* { dg-additional-options { -std=gnu99 -Os -mcall-prologues } } */

#if __SIZEOF_LONG_DOUBLE__ == 8

typedef long double D;
typedef __INT32_TYPE__  int32_t;
typedef __UINT32_TYPE__ uint32_t;
typedef __UINT8_TYPE__  uint8_t;

#define ARRAY_SIZE(X) (sizeof(X) / sizeof(*X))

void testu (void)
{
  static const volatile __flash uint32_t vals[] =
    {
      0, 1ul, -1ul, (-1ul) << 1,
      1ul << 31, 1ul << 30, 1ul << 29, 1ul << 28, 1ul << 27, 1ul << 26,
      1ul << 25, 1ul << 24, 0xff, 123456789
    };

  for (uint8_t i = 0; i < ARRAY_SIZE (vals); ++i)
    {
      D x = (D) vals[i];
      __asm ("" : "+r" (x));
      if ((uint32_t) x != vals[i])
	__builtin_exit (__LINE__);
    }
}

void tests (void)
{
  static const volatile __flash int32_t vals[] =
    {
      0, 1L, -1L, 0x7fffffff, -0x7fffffff, -0x7fffffff - 1,
      -123456789
    };

  for (uint8_t i = 0; i < ARRAY_SIZE (vals); ++i)
    {
      D x = (D) vals[i];
      __asm ("" : "+r" (x));
      if ((int32_t) x != vals[i])
	__builtin_exit (__LINE__);
    }
}

int main (void)
{
  testu ();
  tests ();

  return 0;
}
#else
int main (void)
{
  return 0;
}
#endif
