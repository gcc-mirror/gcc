// { dg-do compile { target c++14 } }

constexpr unsigned long long
foo (unsigned int x, unsigned int y)
{
  unsigned int a = 0;
  unsigned int b = __builtin_clmulg (x, y, &a);
  return (((unsigned long long) a) << ((__SIZEOF_LONG_LONG__ * __CHAR_BIT__) / 2)) | b;
}

constexpr unsigned long long
bar (unsigned int x, unsigned int y)
{
  return __builtin_clmulg ((unsigned long long) x, (unsigned long long) y);
}

#if __SIZEOF_INT__ == 4 && __SIZEOF_LONG_LONG__ == 8 && __CHAR_BIT__ == 8
static_assert (foo (0x10111111U, 0x22002222U) == 0x222022020000202ULL, "");
static_assert (bar (0x10111111U, 0x22002222U) == 0x222022020000202ULL, "");
#endif
static_assert (foo (0, 0) == 0, "");
static_assert (bar (0, 0) == 0, "");
static_assert (foo (6, 4) == 0x18, "");
static_assert (bar (6, 4) == 0x18, "");
