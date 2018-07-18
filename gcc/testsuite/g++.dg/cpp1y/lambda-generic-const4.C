// PR c++/81525
// { dg-do run { target c++14 } }

template <int i> struct A {
  constexpr operator int () const { return i; }
};
template <int i> constexpr A<i> a = {};

template <typename F> void foo (F f) {
  f (A<0>{});
}
template <typename T>
void bar (T) {
  constexpr auto N = a<1>;
  auto f = [&] (auto i) {
    if (static_cast<int>(N) != 1) __builtin_abort();
  };
  foo (f);
}
int main () { bar (0); }
