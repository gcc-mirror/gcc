// PR c++/49369
// { dg-do compile { target c++11 } }

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
