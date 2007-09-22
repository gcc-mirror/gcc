/* { dg-do run } */
/* { dg-options "-std=gnu99 -O0" } */

/* C99 6.5.17: Comma operator.
   Test with fixed-point operands.
   Based on the test from ../dfp/.  */

extern void abort (void);

#define FUNC(TYPE,NAME,PF) \
volatile TYPE NAME ## a, NAME ## b, NAME ## c; \
void \
init_ ## NAME () \
{ \
  NAME ## b = 0.2 ## PF; \
  NAME ## c = 0.3 ## PF; \
}

FUNC (short _Fract, sf, hr)
FUNC (_Fract, f, r)
FUNC (long _Fract, lf, lr)
FUNC (long long _Fract, llf, llr)
FUNC (unsigned short _Fract, usf, uhr)
FUNC (unsigned _Fract, uf, ur)
FUNC (unsigned long _Fract, ulf, ulr)
FUNC (long long _Fract, ullf, ullr)
FUNC (_Sat short _Fract, Ssf, hr)
FUNC (_Sat _Fract, Sf, r)
FUNC (_Sat long _Fract, Slf, lr)
FUNC (_Sat long long _Fract, Sllf, llr)
FUNC (_Sat unsigned short _Fract, Susf, uhr)
FUNC (_Sat unsigned _Fract, Suf, ur)
FUNC (_Sat unsigned long _Fract, Sulf, ulr)
FUNC (_Sat long long _Fract, Sullf, ullr)
FUNC (short _Accum, sa, hk)
FUNC (_Accum, a, k)
FUNC (long _Accum, la, lk)
FUNC (long long _Accum, lla, llk)
FUNC (unsigned short _Accum, usa, uhk)
FUNC (unsigned _Accum, ua, uk)
FUNC (unsigned long _Accum, ula, ulk)
FUNC (long long _Accum, ulla, ullk)
FUNC (_Sat short _Accum, Ssa, hk)
FUNC (_Sat _Accum, Sa, k)
FUNC (_Sat long _Accum, Sla, lk)
FUNC (_Sat long long _Accum, Slla, llk)
FUNC (_Sat unsigned short _Accum, Susa, uhk)
FUNC (_Sat unsigned _Accum, Sua, uk)
FUNC (_Sat unsigned long _Accum, Sula, ulk)
FUNC (_Sat long long _Accum, Sulla, ullk)

int
main ()
{
#define TEST(NAME) \
  init_ ## NAME (); \
  NAME ## a = (NAME ## b, NAME ## c); \
  if (NAME ## a != NAME ## c) \
    abort (); \
  NAME ## a = (NAME ## c, 123, NAME ## b); \
  if (NAME ## a != NAME ## b) \
    abort ();

  TEST(sf)
  TEST(f)
  TEST(lf)
  TEST(llf)
  TEST(usf)
  TEST(uf)
  TEST(ulf)
  TEST(ullf)
  TEST(Ssf)
  TEST(Sf)
  TEST(Slf)
  TEST(Sllf)
  TEST(Susf)
  TEST(Suf)
  TEST(Sulf)
  TEST(Sullf)
  TEST(sa)
  TEST(a)
  TEST(la)
  TEST(lla)
  TEST(usa)
  TEST(ua)
  TEST(ula)
  TEST(ulla)
  TEST(Ssa)
  TEST(Sa)
  TEST(Sla)
  TEST(Slla)
  TEST(Susa)
  TEST(Sua)
  TEST(Sula)
  TEST(Sulla)

  return 0;
}
