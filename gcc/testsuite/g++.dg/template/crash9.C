struct A { };
struct B { };

A f(const B & b) {
  return A();
}

template<>
B f(const A & a) { // { dg-error "" }
  return B();
}

