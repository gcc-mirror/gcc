// { dg-options "-Wall -w" }
// { dg-do compile { target c++11 } }
// { dg-bogus "over-aligned new" "PR c++/80473" { target *-*-* } 0 }

template<typename T> T&& declval();

template<typename T, typename U, typename = void>
struct is_constructible { enum { value = 0 }; };

template<typename T, typename U>
struct is_constructible<T, U, decltype(::new T(declval<U>()), void())>
{ enum { value = 1 }; };

struct alignas(64) A { int i; };

constexpr bool b = is_constructible<A, A>::value;
