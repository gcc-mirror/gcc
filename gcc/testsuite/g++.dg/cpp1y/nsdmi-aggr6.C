// PR c++/77659
// { dg-do compile { target c++14 } }

template <typename Type> Type get_max_value(Type);
struct A {
  struct B {
    int baz = get_max_value(baz);
  };
  template <typename> void m_fn1() { new B{}; }
};
void foo() {
  A a;
  a.m_fn1<int>();
}
