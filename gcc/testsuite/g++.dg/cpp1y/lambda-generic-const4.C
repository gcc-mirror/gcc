// PR c++/81525
// { dg-do compile { target c++14 } }

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
    return static_cast<int>(N) == 1;
  };
  foo (f);
}
int main () { bar (0); }
