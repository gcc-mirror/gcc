/* { dg-do run { target { ! avr_tiny } } } */
/* { dg-additional-options { -std=gnu99 -Os -mcall-prologues } } */

typedef __INT8_TYPE__   int8_t;
typedef __UINT8_TYPE__  uint8_t;
typedef __UINT16_TYPE__ uint16_t;
typedef __UINT64_TYPE__ uint64_t;
typedef __INT64_TYPE__  int64_t;

#define ARRAY_SIZE(X) (sizeof(X) / sizeof(*X))

const __flash uint64_t vals[] =
  {
    // NaNs
    0xffffffffffffffff,
    0x7fffffffffffffff,
    0xfff0000000000001,
    0x7ff0000000000001,

    // Some non-NaN doubles, increasing in magnitude.
    0xfff0000000000000, // -Inf
    0xffefffffffffffff,
    0xffe0000000000000,
    0x8010000000000000,
    0x800fffffffffffff,
    0x800ffffffffffffe,
    0x8007fffffffffffe,
    0x8000000000000001,
    0x0000000000000000,
    0x0000000000000001,
    0x0007fffffffffffe,
    0x000ffffffffffffe,
    0x000fffffffffffff,
    0x0010000000000000,
    0x7fe0000000000000,
    0x7fefffffffffffff,
    0x7ff0000000000000  // +Inf
  };

#define SMASK ((uint64_t) 1 << 63)

char d64_nan_p (uint64_t a)
{
  return (a & ~SMASK) > (uint64_t) 0x7ff << 52;
}

extern uint64_t xmin (uint64_t, uint64_t) __asm("__fmin");
extern uint64_t xmax (uint64_t, uint64_t) __asm("__fmax");

void test_fmin (uint8_t i, uint8_t j)
{
  uint64_t a = vals[i];
  uint64_t b = vals[j];
  uint64_t m = xmin (a, b);

  char a_nan_p = d64_nan_p (a);
  char b_nan_p = d64_nan_p (b);

  if (a_nan_p + b_nan_p == 2)
    {
      if (!d64_nan_p (m))
	__builtin_exit (__LINE__);
    }
  else
    {
      uint64_t r = 0?0
	: a_nan_p ? b
	: b_nan_p ? a
	: i < j ? a : b;
      if (r != m)
	__builtin_exit (__LINE__);
    }
}


void test_fmax (uint8_t i, uint8_t j)
{
  uint64_t a = vals[i];
  uint64_t b = vals[j];
  uint64_t m = xmax (a, b);

  char a_nan_p = d64_nan_p (a);
  char b_nan_p = d64_nan_p (b);

  if (a_nan_p + b_nan_p == 2)
    {
      if (!d64_nan_p (m))
	__builtin_exit (__LINE__);
    }
  else
    {
      uint64_t r = 0?0
	: a_nan_p ? b
	: b_nan_p ? a
	: i > j ? a : b;
      if (r != m)
	__builtin_exit (__LINE__);
    }
}


void tests (void)
{
  for (uint8_t i = 0; i < ARRAY_SIZE (vals); ++i)
    for (uint8_t j = 0; j < ARRAY_SIZE (vals); ++j)
      {
	test_fmin (i, j);
      }
}


int main (void)
{
  tests ();
  return 0;
}
