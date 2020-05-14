// { dg-do compile { target c++20 } }

using L = decltype([]{ });
void f(L) { }
// { dg-final { scan-assembler-not "globl.*_Z1f" } }
