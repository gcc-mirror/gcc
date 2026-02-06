// PR c++/122494
// { dg-do compile { target c++20 } }

template<class T>
concept C = false;

template<class T> requires (!C<T>)
struct A {
  static constexpr unsigned v = 0;
};

template<class T>
struct B {
  static constexpr unsigned v = A<T>::v;

  constexpr static bool f() {
    return [](auto) {
      if (v == 0) { }
      return true;
    }(0);
  }
};

static_assert(B<int>::f());
