/* { dg-do run { target { ! avr_tiny } } } */
/* { dg-additional-options { -std=gnu99 -Os -mcall-prologues } } */

typedef __INT8_TYPE__   int8_t;
typedef __UINT8_TYPE__  uint8_t;
typedef __UINT16_TYPE__ uint16_t;
typedef __UINT64_TYPE__ uint64_t;
typedef __INT64_TYPE__  int64_t;

#define ARRAY_SIZE(X) (sizeof(X) / sizeof(*X))

const __flash uint64_t mant[] =
  {
    0x0000000000000,
    0x0000000000001,
    0x0000000000100,
    0x0000000010000,
    0x0000001000000,
    0x0000100000000,
    0x0010000000000,
    0x1000000000000,
    0x00000000000ff,
    0x00000000000ff,
    0x000000000ffff,
    0x0000000ffffff,
    0x00000ffffffff,
    0x000ffffffffff,
    0x0ffffffffffff,
    0xfffffffffffff,
    0xfffffffffff00,
    0xfffffffff0000,
    0xfffffff000000,
    0xfffff00000000,
    0xfff0000000000,
    0xff00000000000,
    0xf000000000000,
    0x7ffffffffffff,
    0x8000000000000,
    0x8000000000001,
    0xffffffffffffe
  };

const __flash uint16_t expo[] =
  {
    0x000,
    0x001,
    0x002,
    0x7fe,
    0x7ff
  };

#define SMASK ((uint64_t) 1 << 63)
#define xNAN 0x7f

char d64_nan_p (uint64_t a)
{
  return (a & ~SMASK) > (uint64_t) 0x7ff << 52;
}

int8_t cmp_d64 (uint64_t a, uint64_t b)
{
  if (d64_nan_p (a) || d64_nan_p (b))
    return xNAN;

  if (a & SMASK)     a = SMASK - a;
  if (b & SMASK)     b = SMASK - b;
  __asm ("" : "+r" (a));
  __asm ("" : "+r" (b));

  return a == b
    ? 0
    : (int64_t) a > (int64_t) b ? 1 : -1;
}

extern int8_t eq (uint64_t, uint64_t) __asm("__eqdf2");
extern int8_t ne (uint64_t, uint64_t) __asm("__nedf2");
extern int8_t ge (uint64_t, uint64_t) __asm("__gedf2");
extern int8_t gt (uint64_t, uint64_t) __asm("__gtdf2");
extern int8_t le (uint64_t, uint64_t) __asm("__ledf2");
extern int8_t lt (uint64_t, uint64_t) __asm("__ltdf2");
extern int8_t unord (uint64_t, uint64_t) __asm("__unorddf2");

void test1 (uint64_t a, uint64_t b)
{
  int8_t d, c = cmp_d64 (a, b);
  d = eq (a, b);
  if (c == xNAN && d) __builtin_exit (1);
  if (c != xNAN && d != (c == 0)) __builtin_exit (2);

  d = ne (a, b);
  if (c == xNAN && d) __builtin_exit (3);
  if (c != xNAN && d != (c != 0)) __builtin_exit (4);

  d = ge (a, b);
  if (c == xNAN && d) __builtin_exit (5);
  if (c != xNAN && d != (c >= 0)) __builtin_exit (6);

  d = gt (a, b);
  if (c == xNAN && d) __builtin_exit (7);
  if (c != xNAN && d != (c > 0)) __builtin_exit (8);

  d = le (a, b);
  if (c == xNAN && d) __builtin_exit (9);
  if (c != xNAN && d != (c <= 0)) __builtin_exit (10);

  d = lt (a, b);
  if (c == xNAN && d) __builtin_exit (11);
  if (c != xNAN && d != (c < 0)) __builtin_exit (12);

  d = unord (a, b);
  if (c == xNAN && !d) __builtin_exit (13);
  if (c != xNAN && d) __builtin_exit (14);
}


void testAB (uint64_t a, uint64_t b)
{
  test1 (a, b);
  test1 (a, b ^ SMASK);
  test1 (a ^ SMASK, b);
  test1 (a ^ SMASK, b ^ SMASK);
}

void testA (uint64_t a)
{
  for (uint8_t i = 0; i < ARRAY_SIZE (mant); ++i)
    {
      uint64_t b = mant[i];
      for (uint8_t j = 0; j < ARRAY_SIZE (expo); ++j)
	testAB (a, b | ((uint64_t) expo[j] << 52));
    }
}

void tests (void)
{
  for (uint8_t i = 0; i < ARRAY_SIZE (mant); ++i)
    {
      uint64_t a = mant[i];
      for (uint8_t j = 0; j < ARRAY_SIZE (expo); ++j)
	testA (a | ((uint64_t) expo[j] << 52));
    }
}


int main (void)
{
  tests ();
  return 0;
}
