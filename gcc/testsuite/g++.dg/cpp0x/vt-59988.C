// PR c++/59988
// { dg-do compile { target c++11 } }

template<template<typename...> class C, typename... T>
struct is_valid_specialization {
  typedef struct { char _; } yes;
  typedef struct { yes _[2]; } no;

  template<template<typename...> class D, typename... U>
  static yes test(D<U...>*);
  template<template<typename...> class D, typename... U>
  static no test(...);

  constexpr static bool value = (sizeof(test<C, T...>(0)) == sizeof(yes));
};

template<typename T>
struct Test1 { };

template<typename T1, typename T2>
struct Test2 { };

template<typename...>
struct TestV { };

static_assert(!is_valid_specialization<Test1, int>::value, "");
static_assert(!is_valid_specialization<Test2, int>::value, "");
static_assert(!is_valid_specialization<TestV, int>::value, "");
