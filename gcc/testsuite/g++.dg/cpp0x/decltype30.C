// PR c++/49369
// { dg-options -std=c++0x }

template <class,class> struct assert_same;
template <class T> struct assert_same<T,T> {};

struct B {
  int member;
};

struct C: B {
  void method() const;
};

void C::method() const {
  assert_same<decltype((B::member)), const int&> a;
}
