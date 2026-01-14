// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }

void oy (int);
template<typename T>
void oy (T);

struct S {
  void foo (int);
  void foo (int, int);
  void bar (int);

  template<typename T>
  void baz (T);
  template<typename T>
  void baz (T, T);

  template<typename T>
  void qux (T);

  void sfoo (int);
  void sfoo (int, int);
  void sbar (int);

  template<typename T>
  void sbaz (T);
  template<typename T>
  void sbaz (T, T);

  template<typename T>
  void squx (T);

  void lox (int);
  template<typename T>
  void lox (T);

  template<typename T>
  S &operator+(const T&);

  template<typename T>
  static bool B;
};

void
g ()
{
  constexpr auto r1 = ^^S::foo;	  // { dg-error "cannot take the reflection of an overload set" }
  constexpr auto r2 = ^^S::bar;
  constexpr auto r3 = ^^S::baz;	  // { dg-error "cannot take the reflection of an overload set" }
  constexpr auto r4 = ^^S::qux;
  constexpr auto r5 = ^^S::sfoo;  // { dg-error "cannot take the reflection of an overload set" }
  constexpr auto r6 = ^^S::sbar;
  constexpr auto r7 = ^^S::sbaz;  // { dg-error "cannot take the reflection of an overload set" }
  constexpr auto r8 = ^^S::squx;
  constexpr auto r9 = ^^oy;	  // { dg-error "cannot take the reflection of an overload set" }
  constexpr auto r10 = ^^S::lox;  // { dg-error "cannot take the reflection of an overload set" }
  constexpr auto r11 = ^^S::operator+;
  constexpr auto r12 = ^^S::template operator+;
  constexpr auto r13 = ^^S::B;
  constexpr auto r14 = ^^S::template B;
}
