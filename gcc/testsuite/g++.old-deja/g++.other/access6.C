// { dg-do assemble  }

template <int I>
struct S {
  void g();
};

class C {
  static const int i = 3; // { dg-bogus "" } private
public:
  S<C::i>* f(); // { dg-bogus "" } redeclared
};

S<C::i>* C::f() { // { dg-bogus "" } private
  return 0;
}

