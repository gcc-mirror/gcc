// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }

struct A {
  A();
  ~A();
};

void
g ()
{
  // clang accepts these, but EDG rejects too.
  constexpr auto r1 = ^^A::A;	  // { dg-error "cannot take the reflection of an overload set" }
  constexpr auto r2 = ^^A::A();	  // { dg-error "cannot take the reflection of an overload set" }
  constexpr auto r3 = ^^A::A::A;  // { dg-error "cannot take the reflection of an overload set" }
  constexpr auto r4 = ^^A::~A;
  [: r4 :];	  // { dg-error "cannot use constructor or destructor .A::~A\\(\\). in a splice expression" }
  [: ^^A::~A :];  // { dg-error "cannot use constructor or destructor .A::~A\\(\\). in a splice expression" }
}
