/* Test C2x constexpr.  Invalid code, compilation tests, signed char.  */
/* { dg-do compile } */
/* { dg-options "-std=c2x -pedantic-errors -fsigned-char" } */

constexpr unsigned char v3[] = "\x00\xff"; /* { dg-error "'constexpr' initializer not representable in type of object" } */
constexpr char v4[] = u8"\x00\xff"; /* { dg-error "'constexpr' initializer not representable in type of object" } */
constexpr signed char v5[] = u8"\x00\xff"; /* { dg-error "'constexpr' initializer not representable in type of object" } */

void
f0 ()
{
  (constexpr unsigned char []) { "\x00\xff" }; /* { dg-error "'constexpr' initializer not representable in type of object" } */
  (constexpr char []) { u8"\x00\xff" }; /* { dg-error "'constexpr' initializer not representable in type of object" } */
  (constexpr signed char []) { u8"\x00\xff" }; /* { dg-error "'constexpr' initializer not representable in type of object" } */
}
