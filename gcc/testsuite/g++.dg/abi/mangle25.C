// Test mangling of __float128.
// The C++ ABI document says __float128 is mangled as "g".  It
// also says that "long double" is mangled as "e", so these conflict on
// ia64-hpux where "long double" is "e" and __float128 is synonymous with
// "long double".
// Origin: Joseph Myers <joseph@codesourcery.com>
// { dg-do compile { target { ia64-*-* || { { i?86-*-* x86_64-*-*} && lp64 } } } }
// { dg-options "" } */
// { dg-final { scan-assembler "_Z1fg" { target i?86-*-* x86_64-*-* } } } */
// { dg-final { scan-assembler "_Z1fg" { target { ia64-*-* && { ! "ia64-*-hpux*" } } } } } */
// { dg-final { scan-assembler "_Z1fe" { target ia64-*-hpux* } } } */

int f(__float128 x) { return 0; }
