/* { dg-do run } */
/* { dg-options "-std=gnu99" } */

/* C99 6.9.1(9) Function definitions; parameter has automatic storage.

   Test that actual parameters are passed by value and that modifications
   made within functions are lost on function return.

   This test is copied from gcc.dg/dfp/ and changed for fixed-point types.  */

extern void abort (void);

#define FOO_FUN(NAME,TYPE,VALUE) \
int foo ## NAME (TYPE z) \
{ \
  z = z + VALUE; \
}

#define FOO_TEST(NAME,TYPE,VALUE) \
  { \
    TYPE NAME = VALUE; \
    foo ## NAME (NAME); \
    if (NAME != VALUE) \
      abort (); \
  }

FOO_FUN (sf, short _Fract, 0.2hr)
FOO_FUN (f, _Fract, 0.2r)
FOO_FUN (lf, long _Fract, 0.2lr)
FOO_FUN (llf, long long _Fract, 0.2llr)
FOO_FUN (usf, unsigned short _Fract, 0.2uhr)
FOO_FUN (uf, unsigned _Fract, 0.2ur)
FOO_FUN (ulf, unsigned long _Fract, 0.2ulr)
FOO_FUN (ullf, unsigned long long _Fract, 0.2ullr)
FOO_FUN (Ssf, _Sat short _Fract, 0.2hr)
FOO_FUN (Sf, _Sat _Fract, 0.2r)
FOO_FUN (Slf, _Sat long _Fract, 0.2lr)
FOO_FUN (Sllf, _Sat long long _Fract, 0.2llr)
FOO_FUN (Susf, _Sat unsigned short _Fract, 0.2uhr)
FOO_FUN (Suf, _Sat unsigned _Fract, 0.2ur)
FOO_FUN (Sulf, _Sat unsigned long _Fract, 0.2ulr)
FOO_FUN (Sullf, _Sat unsigned long long _Fract, 0.2ullr)
FOO_FUN (sa, short _Accum, 0.2hk)
FOO_FUN (a, _Accum, 0.2k)
FOO_FUN (la, long _Accum, 0.2lk)
FOO_FUN (lla, long long _Accum, 0.2llk)
FOO_FUN (usa, unsigned short _Accum, 0.2uhk)
FOO_FUN (ua, unsigned _Accum, 0.2uk)
FOO_FUN (ula, unsigned long _Accum, 0.2ulk)
FOO_FUN (ulla, unsigned long long _Accum, 0.2ullk)
FOO_FUN (Ssa, _Sat short _Accum, 0.2hk)
FOO_FUN (Sa, _Sat _Accum, 0.2k)
FOO_FUN (Sla, _Sat long _Accum, 0.2lk)
FOO_FUN (Slla, _Sat long long _Accum, 0.2llk)
FOO_FUN (Susa, _Sat unsigned short _Accum, 0.2uhk)
FOO_FUN (Sua, _Sat unsigned _Accum, 0.2uk)
FOO_FUN (Sula, _Sat unsigned long _Accum, 0.2ulk)
FOO_FUN (Sulla, _Sat unsigned long long _Accum, 0.2ullk)

int
main ()
{
  FOO_TEST (sf, short _Fract, 0.2hr)
  FOO_TEST (f, _Fract, 0.2r)
  FOO_TEST (lf, long _Fract, 0.2lr)
  FOO_TEST (llf, long long _Fract, 0.2llr)
  FOO_TEST (usf, unsigned short _Fract, 0.2uhr)
  FOO_TEST (uf, unsigned _Fract, 0.2ur)
  FOO_TEST (ulf, unsigned long _Fract, 0.2ulr)
  FOO_TEST (ullf, unsigned long long _Fract, 0.2ullr)
  FOO_TEST (Ssf, _Sat short _Fract, 0.2hr)
  FOO_TEST (Sf, _Sat _Fract, 0.2r)
  FOO_TEST (Slf, _Sat long _Fract, 0.2lr)
  FOO_TEST (Sllf, _Sat long long _Fract, 0.2llr)
  FOO_TEST (Susf, _Sat unsigned short _Fract, 0.2uhr)
  FOO_TEST (Suf, _Sat unsigned _Fract, 0.2ur)
  FOO_TEST (Sulf, _Sat unsigned long _Fract, 0.2ulr)
  FOO_TEST (Sullf, _Sat unsigned long long _Fract, 0.2ullr)
  FOO_TEST (sa, short _Accum, 0.2hk)
  FOO_TEST (a, _Accum, 0.2k)
  FOO_TEST (la, long _Accum, 0.2lk)
  FOO_TEST (lla, long long _Accum, 0.2llk)
  FOO_TEST (usa, unsigned short _Accum, 0.2uhk)
  FOO_TEST (ua, unsigned _Accum, 0.2uk)
  FOO_TEST (ula, unsigned long _Accum, 0.2ulk)
  FOO_TEST (ulla, unsigned long long _Accum, 0.2ullk)
  FOO_TEST (Ssa, _Sat short _Accum, 0.2hk)
  FOO_TEST (Sa, _Sat _Accum, 0.2k)
  FOO_TEST (Sla, _Sat long _Accum, 0.2lk)
  FOO_TEST (Slla, _Sat long long _Accum, 0.2llk)
  FOO_TEST (Susa, _Sat unsigned short _Accum, 0.2uhk)
  FOO_TEST (Sua, _Sat unsigned _Accum, 0.2uk)
  FOO_TEST (Sula, _Sat unsigned long _Accum, 0.2ulk)
  FOO_TEST (Sulla, _Sat unsigned long long _Accum, 0.2ullk)

  return 0;
}
