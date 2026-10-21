/* { dg-do run { target { bitint && le } } } */

#if __BITINT_MAXWIDTH__ >= 1024
union U1 {
  _BitInt(1024) big;
  struct __attribute__((packed)) { unsigned a : 3; unsigned _BitInt(510) f : 510; } s;
};

union U2 {
  _BitInt(1024) big;
  struct __attribute__((packed)) { unsigned a : 1; unsigned _BitInt(512) f : 512; } s;
};

union U3 {
  _BitInt(1024) big;
  struct __attribute__((packed)) { unsigned a : 5; unsigned _BitInt(508) f : 508; } s;
};

union U4 {
  _BitInt(1024) big;
  struct __attribute__((packed)) { unsigned a : 7; unsigned _BitInt(506) f : 506; } s;
};

__attribute__((noinline)) void
test1 (void)
{
  union U1 u;
  u.big = (_BitInt(1024))1 << 512;
  if (u.s.f >> 509 != 1)
    __builtin_abort ();

  u.big = ((_BitInt(1024))1 << 512) | ((_BitInt(1024))1 << 511) | ((_BitInt(1024))1 << 3);
  if ((u.s.f & 1) != 1)
    __builtin_abort ();
  if ((u.s.f >> 508) != 3)
    __builtin_abort ();
}

__attribute__((noinline)) void
test2 (void)
{
  union U2 u;
  u.big = (_BitInt(1024))1 << 512;
  if (u.s.f >> 511 != 1)
    __builtin_abort ();

  u.big = ((_BitInt(1024))1 << 512) | ((_BitInt(1024))1 << 511) | ((_BitInt(1024))1 << 1);
  if ((u.s.f & 1) != 1)
    __builtin_abort ();
  if ((u.s.f >> 510) != 3)
    __builtin_abort ();
}

__attribute__((noinline)) void
test3 (void)
{
  union U3 u;
  u.big = (_BitInt(1024))1 << 512;
  if (u.s.f >> 507 != 1)
    __builtin_abort ();

  u.big = ((_BitInt(1024))1 << 512) | ((_BitInt(1024))1 << 511) | ((_BitInt(1024))1 << 5);
  if ((u.s.f & 1) != 1)
    __builtin_abort ();
  if ((u.s.f >> 506) != 3)
    __builtin_abort ();
}

__attribute__((noinline)) void
test4 (void)
{
  union U4 u;
  u.big = (_BitInt(1024))1 << 512;
  if (u.s.f >> 505 != 1)
    __builtin_abort ();

  u.big = ((_BitInt(1024))1 << 512) | ((_BitInt(1024))1 << 511) | ((_BitInt(1024))1 << 7);
  if ((u.s.f & 1) != 1)
    __builtin_abort ();
  if ((u.s.f >> 504) != 3)
    __builtin_abort ();
}
#endif

int
main (void)
{
#if __BITINT_MAXWIDTH__ >= 1024
  test1 ();
  test2 ();
  test3 ();
  test4 ();
#endif
  return 0;
}
