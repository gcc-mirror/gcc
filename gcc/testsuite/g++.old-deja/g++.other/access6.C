// Build don't link:

template <int I>
struct S {
  void g();
};

class C {
  static const int i = 3; // gets bogus error - private - XFAIL *-*-*
public:
  S<C::i>* f(); // gets bogus error - redeclared - XFAIL *-*-*
};

S<C::i>* C::f() { // gets bogus error - private - XFAIL *-*-*
  return 0;
}

