// Build don't link:

template <int I>
struct S {
  void g();
};

class C {
  static const int i = 3; // gets bogus error - private
public:
  S<C::i>* f(); // gets bogus error - redeclared
};

S<C::i>* C::f() { // gets bogus error - private
  return 0;
}

