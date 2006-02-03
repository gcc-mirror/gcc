struct A {
};

template <typename T>
struct S : public A {
  using A::operator(); // { dg-error "no member" }
};
