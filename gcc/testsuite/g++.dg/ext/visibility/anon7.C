// PR c++/34094
// { dg-do compile }

namespace
{
  struct A {
    static int bar ();		// { dg-error "used but never defined" }
    static int i;		// { dg-error "used, but not defined" "" { xfail *-*-* } }
    static int j;
    static int k;
    static int l;
    static const int m = 16;
    static const int n = 17;
  };
  int A::j = 4;
  int A::k;
  const int A::m;
}

int foo (void)
{
  return A::i + A::j + A::k + A::m + A::n + A::bar ();
}
