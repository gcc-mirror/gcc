struct A {
  void (*f)(void);
};

template< typename R >
struct B : public A {
  void g()
  {
    A::f();
  }
};
template class B<bool>;
