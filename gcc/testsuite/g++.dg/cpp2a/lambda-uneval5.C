// { dg-do compile { target c++20 } }

using L = decltype([]{ });
void f(L) { }
// { dg-final { scan-assembler-not "\[^l\]globl\[ \t\.\]*_Z1f" } }
