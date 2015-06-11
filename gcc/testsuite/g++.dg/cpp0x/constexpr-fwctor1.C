// PR c++/66450
// { dg-do compile { target c++11 } }

struct foo {
  constexpr foo(int a);
  constexpr foo(int a, int b, int c): a{a}, b{b}, c{c} {}

  int a, b, c;
};

constexpr foo make_foo(int a) { return foo{a, a+1, a+2}; }
constexpr foo::foo(int a): foo{make_foo(a)} {}

int main() {
  constexpr const foo f{3};
  static_assert(f.a == 3, "");
  static_assert(f.b == 4, "");
  static_assert(f.c == 5, "");
}
