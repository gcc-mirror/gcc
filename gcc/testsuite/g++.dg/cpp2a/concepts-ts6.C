// { dg-do compile { target c++2a } }
// { dg-additional-options "-fconcepts-ts" }

template<typename T, int N, typename... Xs> concept bool C1 = true;

template<template<typename> class X> concept bool C2 = true;

template<typename... Ts> concept bool C3 = true;

C1{A, B, ...C} struct S1 { };

C2{T} void f();

C2{...Ts} void g(); // { dg-error "cannot be introduced" }

C3{...Ts} struct S2 { };
C3{T, U, V} struct S3 { };
C3{...Ts, U} struct S4 { }; // { dg-error "cannot deduce template parameters" }

template<typename> struct X { };

void driver1() {
  S1<int, 0, char, bool, float> s1a;
  S1<0, 0, char, bool, float> s1b; // { dg-error "type/value mismatch" }

  f<X>();
  f<int>(); // { dg-error "no matching function for call" }

  S2<int> s2a;
  S2<char, signed char, unsigned char> s2b;
  S2<0> s2c; // { dg-error "type/value mismatch" }

  S3<int, int, int> s3a;
  S3<int, int> s3b; // { dg-error "wrong number of template arguments" }
}

template<typename... Args>
struct all_same;

template<typename T, typename U, typename... Args>
struct all_same<T, U, Args...>
{
  static constexpr bool value = __is_same_as(T, U) && all_same<U, Args...>::value;
};

template<typename T>
struct all_same<T>
{
  static constexpr bool value = true;
};

template<>
struct all_same<>
{
  static constexpr bool value = true;
};

template<typename... Ts>
concept AllSame = all_same<Ts...>::value;

AllSame{...Ts} struct S5 { };
AllSame{T, U} struct S6 { };

void driver2()
{
  S5<int, int> s5a;
  S5<int, int, int, int> s5b;
  S5<int, int, int, char> s5c; // { dg-error "template constraint failure" }
  S6<void, void> s6a;
  S6<void, int> s6c; // { dg-error "template constraint failure" }
  S6<void, void, void> s6b; // { dg-error "wrong number of template arguments" }
}