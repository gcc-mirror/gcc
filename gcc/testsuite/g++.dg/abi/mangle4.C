// Test mangling of type casts
// { dg-do compile }

class A {};
class B : public A {};

template<const A* a> class C {};
template<const B* b> class D {};
template<B* b> class E {};

template<const B* b> void f(D<b> &, C<static_cast<const A*>(b)> &) {} // { dg-error "" }
template<const B* b> void g(D<b> &, E<const_cast<B*>(b)> &) {} // { dg-error "" }

B b;

int main()
{
  C<static_cast<const A*>(&b)> c; // { dg-error "" }
  D<&b> d;
  E<const_cast<B*>(&b)> e; // { dg-error "" }
  f(d, c);
  g(d, e);
}

