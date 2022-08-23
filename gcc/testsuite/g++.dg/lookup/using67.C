// PR c++/104476
// { dg-do compile { target c++11 } }

struct A {
  static void f();
  static void f2();
};

struct B : A
{
  static void f(int);
  using A::f;
  auto g() -> decltype(f());
  auto ga() -> decltype(f(2));

  using A::f2;
  static void f2(int);
  auto g2() -> decltype(f2());
  auto g2a() -> decltype(f2(2));
};
