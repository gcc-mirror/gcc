struct X {
  void f();
};

template <typename T> 
struct S : public T {
  using X::f;
};
