// { dg-do run  }
// Test for composite pointer types, as defined in [expr.rel],
// and common pointer to member types, as defined in [expr.eq].

struct A { int i; };
struct B : public A { };

int main ()
{
  B b;

  // The composite type is `A const *'
        A* ap = &b;
  const B* bp = &b;
  if (ap != bp)		// { dg-bogus "" } distinct types
    return 1;

  // The composite type is `B const *const *'
  B       *const * p = 0;
  B const *      * q = 0;
  if (p != q)		// { dg-bogus "" } distinct types
    return 1;

  // The common type is `int const B::*'
  const int A::*apm = &A::i;
        int B::*bpm = &A::i;
  if (apm != bpm)	// { dg-bogus "" } distinct types
    return 1;
}
