// PR c++/107741

void side_effect();

struct A {
  static constexpr auto x = []{ return 1; };
};

template <typename>
struct B {
  static inline auto x = (side_effect(), []{ return 2; }(), []{ return 3; }());
};

template <typename>
struct C {
  static int x;
};

template <typename T>
int C<T>::x = (side_effect(), []{ return 4; }());

template int C<int>::x;

int f() {
  A::x();
  return B<int>::x;
}

struct D {
  template <typename>
  static constexpr auto x = []{ return 5; };
};

template <typename>
struct E {
  template <typename>
  static inline auto x = (side_effect(), []{ return 6; }(), []{ return 7; }());
};

int g() {
  D::x<int>();
  return E<int>::x<int>;
}
