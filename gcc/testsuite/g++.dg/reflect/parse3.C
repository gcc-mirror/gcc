// PR c++/123823
// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }

constexpr int i = 0;
constexpr auto r = ^^i;

template<auto U, auto>
constexpr int S2 = [:U:];

constexpr auto a1 = S2<[:^^r:],  // { dg-error "unparenthesized splice" }
                       [:^^r:]>; // { dg-error "unparenthesized splice|invalid" }
constexpr auto a2 = S2<([:^^r:]),
                       [:^^r:]>; // { dg-error "unparenthesized splice|invalid" }
constexpr auto a3 = S2<[:^^r:],  // { dg-error "unparenthesized splice" }
                       ([:^^r:])>; // { dg-error "invalid" }
constexpr auto a4 = S2<([:^^r:]),
                       ([:^^r:])>;

template<int>
struct S { };

template<typename>
struct R { };

template<typename T>
constexpr int fn (T) { return 42; }

constexpr int foo (int) { return 42; };

S<[: ^^foo :](0)> s0;
S<template [: ^^fn :](1)> s1;
S<template [: ^^fn :](1) < 43> s2;
S<(template [: ^^fn :](1) > 43)> s3;

template<int N>
constexpr auto var = N;
S<[: ^^var<1> :]> s4; // { dg-error "unparenthesized splice|invalid" }
S<([: ^^var<1> :])> s5;

template<typename T>
struct C {
  static constexpr T t{};
};

template<typename T>
void
f ()
{
  S<template [: ^^C :]<T>::t>();
  R<typename [: ^^C :]<int> >();
  R<typename [: ^^C :]<int>>();
}

void
g ()
{
  f<int> ();
}
