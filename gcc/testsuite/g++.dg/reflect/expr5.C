// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }

struct A {
  static void g();
  static void g(int);
  void h();
  void h(int);
  static void i();
  void j();
};

void
f ()
{
  constexpr auto r1 = ^^A::g; // { dg-error "cannot take the reflection of an overload set" }
  constexpr auto r2 = ^^A::h; // { dg-error "cannot take the reflection of an overload set" }
  constexpr auto r3 = ^^A::i;
  constexpr auto r4 = ^^A::j;

  [: r3 :]();
}
