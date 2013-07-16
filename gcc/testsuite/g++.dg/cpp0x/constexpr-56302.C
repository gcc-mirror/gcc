// PR c++/56302
// { dg-do compile }
// { dg-options "-std=c++11 -O0" }

constexpr int foo () { return 42; }
constexpr int x = foo () + 2;

void
bar ()
{
  __asm ("" : : "n" (x), "n" (foo () * 7 + x));
}
