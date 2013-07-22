/* { dg-options "-std=gnu99" } */
/* { dg-do run } */

#include <stdfix.h>

extern void abort (void);

typedef short _Fract fx_hr_t;
typedef _Fract fx_r_t;
typedef long _Fract fx_lr_t;
typedef long long _Fract fx_llr_t;

typedef unsigned short _Fract fx_uhr_t;
typedef unsigned _Fract fx_ur_t;
typedef unsigned long _Fract fx_ulr_t;
typedef unsigned long long _Fract fx_ullr_t;

typedef short _Accum fx_hk_t;
typedef _Accum fx_k_t;
typedef long _Accum fx_lk_t;
typedef long long _Accum fx_llk_t;

typedef unsigned short _Accum fx_uhk_t;
typedef unsigned _Accum fx_uk_t;
typedef unsigned long _Accum fx_ulk_t;
typedef unsigned long long _Accum fx_ullk_t;


typedef unsigned char int_uhr_t;
typedef unsigned int int_ur_t;
typedef unsigned long int_ulr_t;
typedef unsigned long long int_ullr_t;

typedef unsigned int int_uhk_t;
typedef unsigned long int_uk_t;
typedef unsigned long long int_ulk_t;
typedef unsigned long long int_ullk_t;


#define DEFTEST1(T,FX)                              \
  T test1_##FX (T x, int rp)                        \
  {                                                 \
    return round##FX (x, rp);                       \
  }                                                 \
                                                    \
  unsigned T test1_u##FX (unsigned T x, int rp)     \
  {                                                 \
    return roundu##FX (x, rp);                      \
  }

DEFTEST1 (short fract, hr)
DEFTEST1 (fract, r)
DEFTEST1 (long fract, lr)
DEFTEST1 (long long fract, llr)

DEFTEST1 (short accum, hk)
DEFTEST1 (accum, k)

DEFTEST1 (long accum, lk)
DEFTEST1 (long long accum, llk)


#define TEST2(FX, RP, VAL, ROUND)                                    \
  {                                                                  \
    if (round##FX (FX##bits (VAL), RP) != FX##bits (ROUND))          \
      abort();                                                       \
    fx_##FX##_t (*f)(fx_##FX##_t,int) = round##FX;                   \
    asm ("" : "+r" (f));                                             \
    if (f (FX##bits (VAL), RP) != FX##bits (ROUND))                  \
      abort();                                                       \
  }

static void test2hr (void)
{
  TEST2 (hr, 1, 0x7f, 0x7f);
  TEST2 (hr, 2, 0x70, 0x7f);
  TEST2 (hr, 3, 0x78, 0x7f);
  TEST2 (hr, 4, 0x7f, 0x7f);
 
  TEST2 (uhr, 1, 0x7f, 0x80);
  TEST2 (uhr, 2, 0x7f, 0x80);
  TEST2 (uhr, 3, 0x7f, 0x80);
  TEST2 (uhr, 4, 0x7f, 0x80);
}

void test2k (void)
{
  TEST2 (k, 1, 0x7fffff00, 0x7fffffff);
  TEST2 (k, 2, 0x7ffffff0, 0x7fffffff);
  TEST2 (k, 2, 0x7ffff000, 0x7fffffff);
  TEST2 (k, 3, 0x7ffff000, 0x7ffff000);
  TEST2 (k, 3, 0x7ffff800, 0x7fffffff);
  TEST2 (k, 3, 0x7ffff7ff, 0x7ffff000);
  TEST2 (k, 4, 0x7ffff7ff, 0x7ffff800);

  TEST2 (uk, 1, 0x7fffffff, 1ul << 31);
  TEST2 (uk, 2, 0x7fffffff, 1ul << 31);
  TEST2 (uk, 3, 0x7fffffff, 1ul << 31);
  TEST2 (uk, 4, 0x7fffffff, 1ul << 31);
}

#define DEFTEST3(FX, FBIT)                            \
  void test3##FX (void)                               \
  {                                                   \
    TEST2 (FX, FBIT-1, 0b01100, 0b01100);             \
    TEST2 (FX, FBIT-2, 0b01100, 0b01100);             \
    TEST2 (FX, FBIT-3, 0b01100, 0b10000);             \
    TEST2 (FX, FBIT-4, 0b01100, 0b10000);             \
    TEST2 (FX, FBIT-5, 0b01100, 0);                   \
                                                      \
    if (FX##bits ((int_##FX##_t) -1) > 0)             \
      return;                                         \
                                                      \
    TEST2 (FX, FBIT-1, -0b01100, -0b01100);           \
    TEST2 (FX, FBIT-2, -0b01100, -0b01100);           \
    TEST2 (FX, FBIT-3, -0b01100, -0b01000);           \
    TEST2 (FX, FBIT-4, -0b01100, -0b10000);           \
    TEST2 (FX, FBIT-5, -0b01100, -0b00000);           \
    }

DEFTEST3 (hr, SFRACT_FBIT)
DEFTEST3 (r, FRACT_FBIT)
DEFTEST3 (lr, LFRACT_FBIT)

DEFTEST3 (uhr, USFRACT_FBIT)
DEFTEST3 (ur, UFRACT_FBIT)
DEFTEST3 (ulr, ULFRACT_FBIT)

DEFTEST3 (hk, SACCUM_FBIT)
DEFTEST3 (k, ACCUM_FBIT)
DEFTEST3 (lk, LACCUM_FBIT)
DEFTEST3 (llk, LLACCUM_FBIT)

DEFTEST3 (uhk, USACCUM_FBIT)
DEFTEST3 (uk, UACCUM_FBIT)
DEFTEST3 (ulk, ULACCUM_FBIT)
DEFTEST3 (ullk, ULLACCUM_FBIT)

int main (void)
{
  test2hr();
  test2k();

  test3hr();
  test3r();
  test3lr();

  test3uhr();
  test3ur();
  test3ulr();

  test3hk();
  test3k();
  test3lk();
  test3llk();

  test3uhk();
  test3uk();
  test3ulk();
  test3ullk();

  return 0;
}

