struct B {
  ~B();
};
struct D : public B {
  ~D();
};

void f(D d) {
  d.B::~B();
}
