/* This is like diagnostic-pragma-2.c, but handles the case where everything
   is wrapped inside a macro, which previously caused additional issues tracked
   in PR preprocessor/82335.  */

/* { dg-do compile } */
/* { dg-additional-options "-save-temps -Wattributes -Wtype-limits" } */

#define B _Pragma("GCC diagnostic push") \
          _Pragma("GCC diagnostic ignored \"-Wattributes\"")
#define E _Pragma("GCC diagnostic pop")

#define X() B int __attribute((unknown_attr)) x; E
#define Y   B int __attribute((unknown_attr)) y; E
#define WRAP(x) x

void test1(void)
{
  WRAP(X())
  WRAP(Y)
}

/* Additional test provided on the PR.  */
#define PRAGMA(...) _Pragma(#__VA_ARGS__)
#define PUSH_IGN(X) PRAGMA(GCC diagnostic push) PRAGMA(GCC diagnostic ignored X)
#define POP() PRAGMA(GCC diagnostic pop)
#define TEST(X, Y) \
  PUSH_IGN("-Wtype-limits") \
  int Y = (__typeof(X))-1 < 0; \
  POP()

int test2()
{
  unsigned x;
  TEST(x, i1);
  WRAP(TEST(x, i2))
  return i1 + i2;
}
