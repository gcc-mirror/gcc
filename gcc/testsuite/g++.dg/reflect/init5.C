// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }

using info = decltype(^^void);

struct Base { };
struct Derived : Base {
  info k;
  consteval Derived() : Base(), k(^^int) {}
};
consteval const Base &fn1() {
  static constexpr Derived d;
  return d;
}
constexpr auto &ref = fn1(); // { dg-error "reference into an object of consteval-only type" }

consteval void *fn2() {
  static constexpr auto v = ^^int;
  return (void *)&v;
}
constexpr const void *ptr = fn2(); // { dg-error "pointer into an object of consteval-only type" }
