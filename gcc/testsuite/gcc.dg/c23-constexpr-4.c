/* Test C23 constexpr.  Valid code, compilation tests, signed char.  */
/* { dg-do compile } */
/* { dg-options "-std=c23 -pedantic-errors -fsigned-char" } */

constexpr char v1[] = "\x00\xff";
constexpr signed char v2[] = "\x7f\x80";
constexpr unsigned char v3[] = "\x00\x7f";
constexpr char v4[] = u8"\x00\x7f";
constexpr signed char v5[] = u8"\x7f\x00";
constexpr unsigned char v6[] = u8"\x00\xff";

void
f0 ()
{
  (constexpr char []) { "\x00\xff" };
  (constexpr signed char []) { "\x7f\x80" };
  (constexpr unsigned char []) { "\x00\x7f" };
  (constexpr char []) { u8"\x00\x7f" };
  (constexpr signed char []) { u8"\x7f\x00" };
  (constexpr unsigned char []) { u8"\x00\xff" };
}
