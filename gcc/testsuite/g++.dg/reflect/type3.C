// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test reflections on types.

struct S { int i; };

constexpr auto g1 = ^^int;
constexpr auto g2 = ^^S;

// Create int foo (int, S *);
typename [: g1 :] foo (typename [: g1 :], typename [: g2 :] *);

void
bar (S *s)
{
  foo (42, s);
}

template<typename T>
constexpr int bar (T t) { return t; }

void
g ()
{
  constexpr auto r = [: ^^bar<typename [: ^^int :]> :](42);
  static_assert (r == 42);
}

struct A {
  int a;
  consteval A(int p) : a(p) {}
};
constexpr auto r = ^^A;
struct B : A {
  using [: r :]::A;
  consteval B([: ^^int :] p, [: ^^int :] q) : A(p * q) {}
};
