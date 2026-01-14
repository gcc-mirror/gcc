// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test that we offer some helpful diagnostic.

struct S {
  template<typename T>
  void tfn (T) { }
};

template<typename T>
constexpr T fortytwo = 42;

void
f ()
{
  S s;
  s.[: ^^S::tfn :](42); // { dg-error "reflection .S::tfn. not usable in a splice expression" }
// { dg-message "add .template. to denote a template" "" { target *-*-* } .-1 }
  s.template [: ^^S::tfn :](42);

  constexpr auto r = ^^fortytwo;
  constexpr int i1 = [:r:]<int>; // { dg-error "reflection .fortytwo<int>. not usable in a splice expression with template arguments" }
// { dg-message "add .template. to denote a template" "" { target *-*-* } .-1 }
  constexpr int i2 = template [:r:]<int>;
}
