/* { dg-options "-std=gnu99" } */
/* { dg-do run } */

#include <stdfix.h>

extern void abort (void);

#define DEFTEST1(T,FX)                              \
  int test1_##FX (T x)                              \
  {                                                 \
    return countls##FX (x);                         \
  }                                                 \
                                                    \
  int test1_u##FX (unsigned T x)                    \
  {                                                 \
    return countlsu##FX (x);                        \
  }

DEFTEST1 (short fract, hr)
DEFTEST1 (fract, r)
DEFTEST1 (long fract, lr)
DEFTEST1 (long long fract, llr)

DEFTEST1 (short accum, hk)
DEFTEST1 (accum, k)
DEFTEST1 (long accum, lk)
DEFTEST1 (long long accum, llk)


#define TEST2P(FX, VAL, DD)                                             \
  {                                                                     \
    if (countls##FX (FX##bits (VAL)) != 8 * sizeof (0##FX) - DD)        \
      abort();                                                          \
                                                                        \
    if (countlsu##FX (u##FX##bits (VAL)) != 8 * sizeof (0u##FX) + 1 - DD) \
      abort();                                                          \
  }


#define TEST2M(FX, VAL, DD)                                             \
  {                                                                     \
    if (countls##FX (FX##bits (VAL)) != 8 * sizeof (0##FX) - (DD))      \
      abort();                                                          \
                                                                        \
    if (countlsu##FX (u##FX##bits (VAL)) != 0)                          \
      abort();                                                          \
  }


#define TEST2PX(VAL, DD)                        \
  TEST2P (hr, VAL, DD);                         \
  TEST2P (r,  VAL, DD);                         \
  TEST2P (lr, VAL, DD);                         \
                                                \
  TEST2P (hk, VAL, DD);                         \
  TEST2P (k,  VAL, DD);                         \
  TEST2P (lk, VAL, DD);                         \
  TEST2P (llk, VAL, DD)

#define TEST2MX(VAL, DD)                        \
  TEST2M (hr, VAL,  DD);                        \
  TEST2M (r,  VAL,  DD);                        \
  TEST2M (lr, VAL,  DD);                        \
                                                \
  TEST2M (hk,  VAL, DD);                        \
  TEST2M (k,   VAL, DD);                        \
  TEST2M (lk,  VAL, DD);                        \
  TEST2M (llk, VAL, DD)


int main (void)
{
  TEST2PX (1, 2);
  TEST2PX (2, 3);
  TEST2PX (3, 3);
  
  TEST2MX (-1, 1);
  TEST2MX (-2, 2);
  TEST2MX (-3, 3);
  
  return 0;
}
