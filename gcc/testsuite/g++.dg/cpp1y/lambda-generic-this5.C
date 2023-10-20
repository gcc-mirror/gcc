// PR c++/106086
// { dg-do compile { target c++14 } }

template<class T>
struct A {
  void f(int) const;
  static void g(int);
};

template<class T>
struct B : A<T> {
  auto f() const {
    auto l1 = [&](auto x) { A<T>::f(x); };
    auto l2 = [&](auto x) { A<T>::g(x); };
    static_assert(sizeof(l1) == sizeof(this), "");
    static_assert(sizeof(l2) == 1, "");
    l1(0);
    l2(0);
  }
};

template struct B<void>;
