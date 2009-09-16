/* { dg-do compile } */

/* Test various conversions involving decimal floating types. */

/* Assertion that constant C is of type T.  */
#define ASSERT_CONST_TYPE(C, T)                 \
        do {                                    \
          typedef T type;                       \
          typedef type **typepp;                \
          typedef __typeof__((C)) ctype;        \
          typedef ctype **ctypepp;              \
          typepp x = 0;                         \
          ctypepp y = 0;                        \
          x = y;                                \
          y = x;                                \
        } while (0)

int
main ()
{
  ASSERT_CONST_TYPE (3 + 2.1df, _Decimal32); /* { dg-bogus "assignment from incompatible pointer type" } */
  ASSERT_CONST_TYPE (1.3df + 2, _Decimal32); /* { dg-bogus "assignment from incompatible pointer type" } */
  ASSERT_CONST_TYPE (56U - 55.0dd, _Decimal64); /* { dg-bogus "assignment from incompatible pointer type" } */
  ASSERT_CONST_TYPE (5 * .2DL, _Decimal128); /* { dg-bogus "assignment from incompatible pointer type" } */
  ASSERT_CONST_TYPE (.88dl / 2L, _Decimal128); /* { dg-bogus "assignment from incompatible pointer type" } */
  ASSERT_CONST_TYPE (.114df - 1.6dd, _Decimal64); /* { dg-bogus "assignment from incompatible pointer type" } */
  ASSERT_CONST_TYPE (3L - 1 + .55df, _Decimal32); /* { dg-bogus "assignment from incompatible pointer type" } */

  return 0;
}
