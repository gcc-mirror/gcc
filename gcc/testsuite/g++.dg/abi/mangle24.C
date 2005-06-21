// Test mangling of __float80.
// The C++ ABI document says __float80 is mangled as "e".  It
// also says that "long double" is mangled as "e", so these conflict on
// ia64-hpux where "long double" is "e" and __float80 is "u9__float80".
// Origin: Joseph Myers <joseph@codesourcery.com>
// { dg-do compile { target i?86-*-* x86_64-*-* ia64-*-* } } */
// { dg-options "" } */
// { dg-options "-mmmx" { target { i?86-*-* && ilp32 } } } */
// { dg-options "-mmmx" { target { x86_64-*-* && ilp32 } } } */
// { dg-final { scan-assembler "_Z1fe" { target i?86-*-* x86_64-*-* } } } */
// { dg-final { scan-assembler "_Z1fe" { target { ia64-*-* && { ! "ia64-*-hpux*" } } } } } */
// { dg-final { scan-assembler "_Z1fu9__float80" { target ia64-*-hpux* } } } */

int f(__float80 x) { return 0; }
