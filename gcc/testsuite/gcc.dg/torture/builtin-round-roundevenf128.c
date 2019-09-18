/* { dg-do link } */
/* { dg-add-options float128 } */
/* { dg-require-effective-target float128 } */

extern int link_error (int);

#define TEST(FN, VALUE, RESULT) \
  if (__builtin_##FN##f128 (VALUE) != RESULT) link_error (__LINE__);

int
main (void)
{
  TEST(roundeven,  (0x1p64+0.5f128), (0x1p64f128));
  TEST(roundeven,  (0x1p63+0.5f128), (0x1p63f128));
  TEST(roundeven,  (0x1p63-0.5f128), (0x1p63f128));
  TEST(roundeven,  (0x1p64-0.5f128), (0x1p64f128));
  TEST(roundeven,  (0x1p64+0.501f128), (0x1p64+1.0f128));
  TEST(roundeven,  (0x1.C00000000000039A5653p1f128), (0x1p2f128))
  return 0;
}

