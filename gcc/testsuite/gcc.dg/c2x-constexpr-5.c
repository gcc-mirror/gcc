/* Test C2x constexpr.  Valid code, compilation tests, unsigned char.  */
/* { dg-do compile } */
/* { dg-options "-std=c2x -pedantic-errors -funsigned-char" } */

constexpr char v1[] = "\x00\xff";
constexpr signed char v2[] = "\x7f\x00";
constexpr unsigned char v3[] = "\x80\x7f";
constexpr char v4[] = u8"\x00\xff";
constexpr signed char v5[] = u8"\x7f\x00";
constexpr unsigned char v6[] = u8"\x00\xff";

void
f0 ()
{
  (constexpr char []) { "\x00\xff" };
  (constexpr signed char []) { "\x7f\x00" };
  (constexpr unsigned char []) { "\x80\x7f" };
  (constexpr char []) { u8"\x00\xff" };
  (constexpr signed char []) { u8"\x7f\x00" };
  (constexpr unsigned char []) { u8"\x00\xff" };
}
