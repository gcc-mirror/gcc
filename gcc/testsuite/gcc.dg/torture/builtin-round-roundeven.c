/* { dg-do link } */

extern int link_error (int);

#define TEST(FN, VALUE, RESULT) \
  if (__builtin_##FN (VALUE) != RESULT) link_error (__LINE__);

int
main (void)
{
  TEST(roundeven,  0, 0);
  TEST(roundeven,  0.5, 0);
  TEST(roundeven,  -0.5, 0);
  TEST(roundeven,  6, 6);
  TEST(roundeven,  -8, -8);
  TEST(roundeven,  2.5, 2);
  TEST(roundeven,  3.5, 4);
  TEST(roundeven,  -1.5, -2);
  TEST(roundeven,  3.499, 3);
  TEST(roundeven,  3.501, 4);

  if (__builtin_copysign (1, __builtin_roundeven (-0.5)) != -1)
    link_error (__LINE__);
  if (__builtin_copysign (1, __builtin_roundeven (-0.0)) != -1)
    link_error (__LINE__);
  if (__builtin_copysign (-1, __builtin_roundeven (0.5)) != 1)
    link_error (__LINE__);
  if (__builtin_copysign (-1, __builtin_roundeven (0.0)) != 1)
    link_error (__LINE__);
  if (__builtin_copysign (1, __builtin_roundeven (-0.25)) != -1)
    link_error (__LINE__);
  if (__builtin_copysign (-1, __builtin_roundeven (0.25)) != 1)
    link_error (__LINE__);
 return 0;
}

