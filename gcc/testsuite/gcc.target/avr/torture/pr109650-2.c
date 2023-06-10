/* { dg-do run } */

typedef __UINT8_TYPE__ uint8_t;

#define AI static __inline__ __attribute__((__always_inline__))
#define NI __attribute__((__noinline__,__noclone__))

AI uint8_t func1_eq (uint8_t c, unsigned x)
{
  if (x == c)
    return 1;
  return 0;
}

AI uint8_t func1_ne (uint8_t c, unsigned x)
{
  if (x != c)
    return 1;
  return 0;
}

AI uint8_t func1_ltu (uint8_t c, unsigned x)
{
  if (x < c)
    return 1;
  return 0;
}

AI uint8_t func1_leu (uint8_t c, unsigned x)
{
  if (x <= c)
    return 1;
  return 0;
}

AI uint8_t func1_gtu (uint8_t c, unsigned x)
{
  if (x > c)
    return 1;
  return 0;
}

AI uint8_t func1_geu (uint8_t c, unsigned x)
{
  if (x >= c)
    return 1;
  return 0;
}

NI uint8_t func2_eq (uint8_t c, unsigned x) { return func1_eq (c, x); }
NI uint8_t func2_ne (uint8_t c, unsigned x) { return func1_ne (c, x); }
NI uint8_t func2_ltu (uint8_t c, unsigned x) { return func1_ltu (c, x); }
NI uint8_t func2_leu (uint8_t c, unsigned x) { return func1_leu (c, x); }
NI uint8_t func2_gtu (uint8_t c, unsigned x) { return func1_gtu (c, x); }
NI uint8_t func2_geu (uint8_t c, unsigned x) { return func1_geu (c, x); }

AI void test4 (uint8_t c, unsigned x)
{
  if (func2_eq (c, x) != func1_eq (c, x)) __builtin_abort();
  if (func2_ne (c, x) != func1_ne (c, x)) __builtin_abort();
  if (func2_ltu (c, x) != func1_ltu (c, x)) __builtin_abort();
  if (func2_leu (c, x) != func1_leu (c, x)) __builtin_abort();
  if (func2_gtu (c, x) != func1_gtu (c, x)) __builtin_abort();
  if (func2_geu (c, x) != func1_geu (c, x)) __builtin_abort();
}

int main (void)
{
  test4 (127, 127);
  test4 (127, 128);
  test4 (128, 127);

  test4 (0x42, 0x142);
  test4 (0x0, 0x100);
  test4 (0x0, 0x0);
  test4 (0x0, 0x1);

  __builtin_exit (0);
}
