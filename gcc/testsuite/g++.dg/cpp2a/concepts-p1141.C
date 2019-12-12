// { dg-do compile }
// { dg-options "-std=c++2a" }

template<typename T>
concept Type = true;

template<typename T>
concept Bottom = false;

template<typename T>
concept Class = __is_class(T);

template<auto N>
concept Number = true;

template<template<typename> class T>
concept Template = true;

struct empty { };

Type x1 = 0; // { dg-error "expected 'auto'" }
Type auto x2 = 0;

Number x3 = 0; // { dg-error "does not constrain a type" }
Template x4 = 0; // { dg-error "does not constrain a type" }

Type const& x5 = 0; // { dg-error "expected 'auto'" }
const Type& x6 = 0; // { dg-error "expected 'auto'" }
Type auto const& x7 = 0;
const Type auto& x8 = 0;
Type const auto& x9 = 0; // { dg-error "expected 'auto'|two or more data types" }

template<Type T> // OK: T is a type parameter.
void f1(T);

template<Number N> // { dg-error "does not constrain a type" }
void f2();

template<Template X> // { dg-error "does not constrain a type" }
void f3();

template<Type auto N> // OK: N is a non-type parameter.
void f4() { }

template<Bottom auto N> // OK: but never usable.
void f5();

void driver() {
  f4<0>();
  f5<0>(); // { dg-error "no matching function for call | constraints not satisfied" }
}

Type f6() { return 0; } // { dg-error "expected 'auto'" }
static_assert(__is_same_as(decltype(f6), int()));

Type auto f7() { return 0; }
static_assert(__is_same_as(decltype(f7), int()));

Type f8() { return 0; } // { dg-error "expected 'auto'" }
auto f9() -> Type { return 0; } // { dg-error "expected 'auto'" }

Type f10() { } // { dg-error "expected 'auto'" }
auto f11() -> Type { } // { dg-error "expected" }

Number f12(); // { dg-error "does not constrain a type" }
auto f13() -> Number; // { dg-error "does not constrain a type" }

Template f14(); // { dg-error "does not constrain a type" }
auto f15() -> Template; // { dg-error "does not constrain a type" }

Type f16() { return 0; } // { dg-error "expected 'auto'" }
auto f17() -> Type { return 0; } // { dg-error "expected 'auto'" }

// Abbreviated function templates

void f18(Class x) { } // { dg-error "expected 'auto'" }
void f19(Class auto x) { }
void f20(Class auto x, Class auto y) { }

void driver_1()
{
  f19(0); // { dg-error "" }
  f19(empty{});
  f20(0, empty{}); // { dg-error "" }
  f20(empty{}, empty{});
}

// Abbreviated function redeclarations

// Functionally equivalent but not equivalent.
void f21(Class auto x);
template<Class T> void f21(T x);

void driver_2()
{
  f21(empty{}); // { dg-error "call of overload | ambiguous" }
}

