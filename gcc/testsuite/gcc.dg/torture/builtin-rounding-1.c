/* Copyright (C) 2004 Free Software Foundation.

   Check that constant folding of the rounding math functions doesn't
   break anything and produces the expected results.

   Written by Kaveh Ghazi, 2004-04-29.  */

/* { dg-do link } */

#define PROTOTYPE(FN) \
  PROTOTYPE_LINK_ERROR(FN) \
  extern double FN (double); \
  extern float FN##f (float); \
  extern long double FN##l (long double);

#define PROTOTYPE_RET(FN, RET) \
  PROTOTYPE_LINK_ERROR(FN) \
  extern RET FN (double); \
  extern RET FN##f (float); \
  extern RET FN##l (long double);

#define PROTOTYPE_LINK_ERROR(FN) \
  extern void link_error_##FN(void); \
  extern void link_error_##FN##f(void); \
  extern void link_error_##FN##l(void);

#define TEST(FN, VALUE, RESULT) \
  if (FN (VALUE) != RESULT) link_error_##FN(); \
  if (FN##f (VALUE) != RESULT) link_error_##FN##f(); \
  if (FN##l (VALUE) != RESULT) link_error_##FN##l(); \

PROTOTYPE (trunc);
PROTOTYPE (floor);
PROTOTYPE (ceil);
PROTOTYPE (round);
PROTOTYPE_RET (lround, long);
PROTOTYPE_RET (llround, long long);

int
main (void)
{
  TEST(trunc,   0, 0);
  TEST(floor,   0, 0);
  TEST(ceil,    0, 0);
  TEST(round,   0, 0);
  TEST(lround,  0, 0);
  TEST(llround, 0, 0);
  
  TEST(trunc,   6, 6);
  TEST(floor,   6, 6);
  TEST(ceil,    6, 6);
  TEST(round,   6, 6);
  TEST(lround,  6, 6);
  TEST(llround, 6, 6);
  
  TEST(trunc,   -8, -8);
  TEST(floor,   -8, -8);
  TEST(ceil,    -8, -8);
  TEST(round,   -8, -8);
  TEST(lround,  -8, -8);
  TEST(llround, -8, -8);
  
  TEST(trunc,   3.2, 3);
  TEST(floor,   3.2, 3);
  TEST(ceil,    3.2, 4);
  TEST(round,   3.2, 3);
  TEST(lround,  3.2, 3);
  TEST(llround, 3.2, 3);

  TEST(trunc,   -2.8, -2);
  TEST(floor,   -2.8, -3);
  TEST(ceil,    -2.8, -2);
  TEST(round,   -2.8, -3);
  TEST(lround,  -2.8, -3);
  TEST(llround, -2.8, -3);

  TEST(trunc,   0.01, 0);
  TEST(floor,   0.01, 0);
  TEST(ceil,    0.01, 1);
  TEST(round,   0.01, 0);
  TEST(lround,  0.01, 0);
  TEST(llround, 0.01, 0);

  TEST(trunc,   -0.7, 0);
  TEST(floor,   -0.7, -1);
  TEST(ceil,    -0.7, 0);
  TEST(round,   -0.7, -1);
  TEST(lround,  -0.7, -1);
  TEST(llround, -0.7, -1);

  TEST(trunc,   2.5, 2);
  TEST(floor,   2.5, 2);
  TEST(ceil,    2.5, 3);
  TEST(round,   2.5, 3);
  TEST(lround,  2.5, 3);
  TEST(llround, 2.5, 3);

  TEST(trunc,   -1.5, -1);
  TEST(floor,   -1.5, -2);
  TEST(ceil,    -1.5, -1);
  TEST(round,   -1.5, -2);
  TEST(lround,  -1.5, -2);
  TEST(llround, -1.5, -2);

  return 0;
}
