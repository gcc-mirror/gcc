// PR c++/89612
// { dg-do compile { target c++11 } }

template <typename T>
struct C {
  template <int N>
  friend void foo(T t) noexcept(sizeof(decltype(t)) > 1);

  template <int N>
  friend void foo2(T t) noexcept(sizeof(decltype(t)) < 1); // { dg-error "different exception" }
};

template <int N>
void foo(int i) noexcept { }

template <int N>
void foo2(int i) noexcept { }

C<int> c;
