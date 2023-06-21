// PR c++/110122
// { dg-do compile { target c++20 } }

struct Foo {
  Foo() = default;
  constexpr Foo(const Foo&) { }
};

struct Bar {
  Foo _;
};

template<Bar V>
struct A { };

template<Bar V>
void f() {
  [](auto){ A<V> d; }(0); // { dg-bogus "used before its definition" }
};

int main() {
  f<Bar{}>();
}
