// PR c++/96330
// { dg-do compile { target c++14 } }

struct foo_t {
  template <class T> static constexpr bool bar = true;
};
constexpr foo_t foo{};

template <class T>
void f() {
  int x = foo.bar<T>;
  int y = foo_t::bar<T>;
}

template void f<int>();
