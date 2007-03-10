// { dg-options "-std=gnu++0x" }
template<typename T> struct add_pointer;
template<typename T> struct add_reference;

template<template<class T> class... Metafunctions>
struct metatuple {
  static const int value = 0;
};

template<>
struct metatuple<add_pointer> {
  static const int value = 1;
};

template<template<class T> class Meta>
struct metatuple<Meta, Meta> { // { dg-error "candidates" }
  static const int value = 2;
};

template<template<class T> class... Metafunctions>
struct metatuple<add_pointer, Metafunctions...> { // { dg-error "" }
  static const int value = 3;
};

template<template<class T> class First,
         template<class T> class... Metafunctions>
struct metatuple<First, Metafunctions...> { // { dg-error "struct" }
  static const int value = 4;
};

template<template<class T> class First,
         template<class T> class Second,
         template<class T> class... Metafunctions>
struct metatuple<First, Second, Metafunctions...> { // { dg-error "struct" }
  static const int value = 5;
};

int a0[metatuple<>::value == 0? 1 : -1];
int a1[metatuple<add_pointer>::value == 1? 1 : -1];
int a2a[metatuple<add_pointer, add_pointer>::value == 2? 1 : -1]; // { dg-error "ambiguous" }
int a2b[metatuple<add_reference, add_reference>::value == 2? 1 : -1];
int a3[metatuple<add_pointer, add_reference>::value == 3? 1 : -1]; // { dg-error "ambiguous" }
int a4[metatuple<add_reference>::value == 4? 1 : -1];
int a5[metatuple<add_reference, add_pointer>::value == 5? 1 : -1];

// { dg-error "incomplete" "" { target *-*-* } 40 }
// { dg-error "incomplete" "" { target *-*-* } 42 }
