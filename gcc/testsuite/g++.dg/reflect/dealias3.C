// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test std::meta::dealias.

#include <meta>

using namespace std::meta;

template<typename T>
struct S {};
static_assert (dealias (^^S) == ^^S);

template <typename T> using A1 = T;
static_assert (dealias (^^A1) == ^^A1);

template <typename T> using A2 = S<T>;
static_assert (dealias (^^A2) == ^^A2);

template <typename T> using A3 = S<T*>;
static_assert (dealias (^^A3) == ^^A3);

template <typename T>
int v = sizeof(T);
static_assert (dealias (^^v) == ^^v);

template <typename T>
void fn();
static_assert (dealias (^^fn) == ^^fn);

template<typename T>
concept C = true;
static_assert (dealias (^^C) == ^^C);
