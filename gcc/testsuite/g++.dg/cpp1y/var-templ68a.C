// PR c++/96330
// { dg-do compile { target c++14 } }

template <class>
struct foo_t {
  template <class T> static constexpr bool bar = true;
};
template <class T> constexpr foo_t<T> foo{};

template <class T>
void f() {
  int x = foo<T>.template bar<T>;
  int y = foo_t<T>::template bar<T>;
}

template void f<int>();
