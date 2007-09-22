/* { dg-do run } */
/* { dg-options "-std=gnu99" } */

/* C99 6.5.2.2 Function calls.
   Test scalar passing and return values involving fixed-point types.
   Based on the test from ../dfp/.  */

extern void abort (void);

/* A handful of functions that return the Nth argument of
   an incoming array.  */

#define FUNC(TYPE,NAME) \
TYPE NAME ## 0 (TYPE arg0, int arg1, unsigned arg2, float arg3, double arg4) \
  { return arg0; } \
TYPE NAME ## 1 (int arg0, TYPE arg1, unsigned arg2, float arg3, double arg4) \
  { return arg1; } \
TYPE NAME ## 2 (int arg0, unsigned arg1, TYPE arg2, float arg3, double arg4) \
  { return arg2; } \
TYPE NAME ## 3 (int arg0, unsigned arg1, float arg2, TYPE arg3, double arg4) \
  { return arg3; } \
TYPE NAME ## 4 (int arg0, unsigned arg1, float arg2, double arg3, TYPE arg4) \
  { return arg4; }

FUNC (short _Fract, sf)
FUNC (_Fract, f)
FUNC (long _Fract, lf)
FUNC (long long _Fract, llf)
FUNC (unsigned short _Fract, usf)
FUNC (unsigned _Fract, uf)
FUNC (unsigned long _Fract, ulf)
FUNC (unsigned long long _Fract, ullf)
FUNC (_Sat short _Fract, Ssf)
FUNC (_Sat _Fract, Sf)
FUNC (_Sat long _Fract, Slf)
FUNC (_Sat long long _Fract, Sllf)
FUNC (_Sat unsigned short _Fract, Susf)
FUNC (_Sat unsigned _Fract, Suf)
FUNC (_Sat unsigned long _Fract, Sulf)
FUNC (_Sat unsigned long long _Fract, Sullf)
FUNC (short _Accum, sa)
FUNC (_Accum, a)
FUNC (long _Accum, la)
FUNC (long long _Accum, lla)
FUNC (unsigned short _Accum, usa)
FUNC (unsigned _Accum, ua)
FUNC (unsigned long _Accum, ula)
FUNC (unsigned long long _Accum, ulla)
FUNC (_Sat short _Accum, Ssa)
FUNC (_Sat _Accum, Sa)
FUNC (_Sat long _Accum, Sla)
FUNC (_Sat long long _Accum, Slla)
FUNC (_Sat unsigned short _Accum, Susa)
FUNC (_Sat unsigned _Accum, Sua)
FUNC (_Sat unsigned long _Accum, Sula)
FUNC (_Sat unsigned long long _Accum, Sulla)

int main()
{
#define TEST(TYPE,NAME,PF) \
  { \
    if (NAME ## 0 (0.1 ## PF, -1, 1, 0.3f, 0.5) != 0.1 ## PF) abort (); \
    if (NAME ## 1 (-1, 0.1 ## PF, 1, 0.3f, 0.5) != 0.1 ## PF) abort (); \
    if (NAME ## 2 (-1, 1, 0.1 ## PF, 0.3f, 0.5) != 0.1 ## PF) abort (); \
    if (NAME ## 3 (-1, 1, 0.3f, 0.1 ## PF, 0.5) != 0.1 ## PF) abort (); \
    if (NAME ## 4 (-1, 1, 0.3f, 0.5, 0.1 ## PF) != 0.1 ## PF) abort (); \
  }

  TEST (short _Fract, sf, hr)
  TEST (_Fract, f, r)
  TEST (long _Fract, lf, lr)
  TEST (long long _Fract, llf, llr)
  TEST (unsigned short _Fract, usf, uhr)
  TEST (unsigned _Fract, uf, ur)
  TEST (unsigned long _Fract, ulf, ulr)
  TEST (unsigned long long _Fract, ullf, ullr)
  TEST (_Sat short _Fract, Ssf, hr)
  TEST (_Sat _Fract, Sf, r)
  TEST (_Sat long _Fract, Slf, lr)
  TEST (_Sat long long _Fract, Sllf, llr)
  TEST (_Sat unsigned short _Fract, Susf, uhr)
  TEST (_Sat unsigned _Fract, Suf, ur)
  TEST (_Sat unsigned long _Fract, Sulf, ulr)
  TEST (_Sat unsigned long long _Fract, Sullf, ullr)
  TEST (short _Accum, sa, hk)
  TEST (_Accum, a, k)
  TEST (long _Accum, la, lk)
  TEST (long long _Accum, lla, llk)
  TEST (unsigned short _Accum, usa, uhk)
  TEST (unsigned _Accum, ua, uk)
  TEST (unsigned long _Accum, ula, ulk)
  TEST (unsigned long long _Accum, ulla, ullk)
  TEST (_Sat short _Accum, Ssa, hk)
  TEST (_Sat _Accum, Sa, k)
  TEST (_Sat long _Accum, Sla, lk)
  TEST (_Sat long long _Accum, Slla, llk)
  TEST (_Sat unsigned short _Accum, Susa, uhk)
  TEST (_Sat unsigned _Accum, Sua, uk)
  TEST (_Sat unsigned long _Accum, Sula, ulk)
  TEST (_Sat unsigned long long _Accum, Sulla, ullk)

  return 0;
}
