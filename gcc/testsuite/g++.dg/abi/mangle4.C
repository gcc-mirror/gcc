// Test mangling of type casts
// { dg-do compile }

class A {};
class B : A {};

template<const A* a> class C {};
template<const B* b> class D {};
template<B* b> class E {};

template<const B* b> void f(D<b> &, C<static_cast<const A*>(b)> &) {}
template<const B* b> void g(D<b> &, E<const_cast<B*>(b)> &) {}

B b;

int main()
{
  C<static_cast<const A*>(&b)> c;
  D<&b> d;
  E<const_cast<B*>(&b)> e;
  f(d, c);
  g(d, e);
}

// { dg-final { scan-assembler "\n_?_Z1fIXadL_Z1bEEEvR1DIXT_EER1CIXcvPK1AT_EE\[: \t\n\]" } }
// { dg-final { scan-assembler "\n_?_Z1gIXadL_Z1bEEEvR1DIXT_EER1EIXcvP1BT_EE\[: \t\n\]" } }
