/* PR c/51712 */
/* { dg-do compile } */
/* { dg-options "-Wtype-limits" } */

enum test_enum {
  FOO,
  BAR
};

int valid(enum test_enum arg)
{
  return arg >= 0 && arg <= BAR;
}

int valid2(unsigned int arg2)
{
  return arg2 >= FOO && arg2 <= BAR; /* { dg-bogus "comparison of unsigned expression" "" { xfail *-*-* } } */
}
