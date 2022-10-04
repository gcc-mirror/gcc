// PR c++/107154
// { dg-do compile { target c++11 } }
// { dg-additional-options "-gno-as-loc-support -dA" }
// Test that we emit debug info exactly once for the last line.
// { dg-final { scan-assembler-times {:25:1} 1 } }

bool dummy;

struct S {
  const char *p;
  S(const char *p): p(p) {}
  ~S() { dummy = true; }
};

using Sar = S[];

struct X {
  X(Sar&&) { }
};

int main()
{
  X x(Sar{"", ""});
  return 0;
}
