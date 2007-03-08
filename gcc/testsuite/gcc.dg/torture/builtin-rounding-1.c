/* Copyright (C) 2004 Free Software Foundation.

   Check that constant folding of the rounding math functions doesn't
   break anything and produces the expected results.

   Written by Kaveh Ghazi, 2004-04-29.  */

/* { dg-do link } */

extern int link_error (int);

#define TEST(FN, VALUE, RESULT) \
  if (__builtin_##FN (VALUE) != RESULT) link_error (__LINE__); \
  if (__builtin_##FN##f (VALUE) != RESULT) link_error (__LINE__); \
  if (__builtin_##FN##l (VALUE) != RESULT) link_error (__LINE__); \

int
main (void)
{
  TEST(trunc,   0, 0);
  TEST(floor,   0, 0);
  TEST(ceil,    0, 0);
  TEST(round,   0, 0);
  TEST(lround,  0, 0);
  TEST(llround, 0, 0);
  TEST(lfloor,  0, 0);
  TEST(llfloor, 0, 0);
  TEST(lceil,  0, 0);
  TEST(llceil, 0, 0);
  
  TEST(trunc,   6, 6);
  TEST(floor,   6, 6);
  TEST(ceil,    6, 6);
  TEST(round,   6, 6);
  TEST(lround,  6, 6);
  TEST(llround, 6, 6);
  TEST(lfloor,  6, 6);
  TEST(llfloor, 6, 6);
  TEST(lceil,  6, 6);
  TEST(llceil, 6, 6);
  
  TEST(trunc,   -8, -8);
  TEST(floor,   -8, -8);
  TEST(ceil,    -8, -8);
  TEST(round,   -8, -8);
  TEST(lround,  -8, -8);
  TEST(llround, -8, -8);
  TEST(lfloor,  -8, -8);
  TEST(llfloor, -8, -8);
  TEST(lceil,  -8, -8);
  TEST(llceil, -8, -8);
  
  TEST(trunc,   3.2, 3);
  TEST(floor,   3.2, 3);
  TEST(ceil,    3.2, 4);
  TEST(round,   3.2, 3);
  TEST(lround,  3.2, 3);
  TEST(llround, 3.2, 3);
  TEST(lfloor,  3.2, 3);
  TEST(llfloor, 3.2, 3);
  TEST(lceil,  3.2, 4);
  TEST(llceil, 3.2, 4);

  TEST(trunc,   -2.8, -2);
  TEST(floor,   -2.8, -3);
  TEST(ceil,    -2.8, -2);
  TEST(round,   -2.8, -3);
  TEST(lround,  -2.8, -3);
  TEST(llround, -2.8, -3);
  TEST(lfloor,  -2.8, -3);
  TEST(llfloor, -2.8, -3);
  TEST(lceil,  -2.8, -2);
  TEST(llceil, -2.8, -2);

  TEST(trunc,   0.01, 0);
  TEST(floor,   0.01, 0);
  TEST(ceil,    0.01, 1);
  TEST(round,   0.01, 0);
  TEST(lround,  0.01, 0);
  TEST(llround, 0.01, 0);
  TEST(lfloor,  0.01, 0);
  TEST(llfloor, 0.01, 0);
  TEST(lceil,  0.01, 1);
  TEST(llceil, 0.01, 1);

  TEST(trunc,   -0.7, 0);
  TEST(floor,   -0.7, -1);
  TEST(ceil,    -0.7, 0);
  TEST(round,   -0.7, -1);
  TEST(lround,  -0.7, -1);
  TEST(llround, -0.7, -1);
  TEST(lfloor,  -0.7, -1);
  TEST(llfloor, -0.7, -1);
  TEST(lceil,  -0.7, 0);
  TEST(llceil, -0.7, 0);

  TEST(trunc,   2.5, 2);
  TEST(floor,   2.5, 2);
  TEST(ceil,    2.5, 3);
  TEST(round,   2.5, 3);
  TEST(lround,  2.5, 3);
  TEST(llround, 2.5, 3);
  TEST(lfloor,  2.5, 2);
  TEST(llfloor, 2.5, 2);
  TEST(lceil,  2.5, 3);
  TEST(llceil, 2.5, 3);

  TEST(trunc,   -1.5, -1);
  TEST(floor,   -1.5, -2);
  TEST(ceil,    -1.5, -1);
  TEST(round,   -1.5, -2);
  TEST(lround,  -1.5, -2);
  TEST(llround, -1.5, -2);
  TEST(lfloor,  -1.5, -2);
  TEST(llfloor, -1.5, -2);
  TEST(lceil,  -1.5, -1);
  TEST(llceil, -1.5, -1);

  return 0;
}
