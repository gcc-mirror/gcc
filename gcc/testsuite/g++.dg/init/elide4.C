// PR c++/67557

class A {
public:
  A m_fn1();
  A(A const &);
  int *L;
  int ref;
};
struct B : A {
  B();
};
B::B() : A((0, m_fn1())) {}
