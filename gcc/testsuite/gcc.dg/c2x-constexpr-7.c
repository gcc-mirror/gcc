/* Test C23 constexpr.  Invalid code, compilation tests, unsigned char.  */
/* { dg-do compile } */
/* { dg-options "-std=c23 -pedantic-errors -funsigned-char" } */

constexpr signed char v2[] = "\x00\xff"; /* { dg-error "'constexpr' initializer not representable in type of object" } */
constexpr signed char v5[] = u8"\x00\xff"; /* { dg-error "'constexpr' initializer not representable in type of object" } */

void
f0 ()
{
  (constexpr signed char []) { "\x00\xff" }; /* { dg-error "'constexpr' initializer not representable in type of object" } */
  (constexpr signed char []) { u8"\x00\xff" }; /* { dg-error "'constexpr' initializer not representable in type of object" } */
}
