// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test from the P2996 paper.

#include <vector>

template <typename T> void fn() requires (^^T != ^^int);
template <typename T> void fn() requires (^^T == ^^int);
template <typename T> void fn() requires (sizeof(T) == sizeof(int));

constexpr auto a = ^^fn<char>;     // OK
constexpr auto b = ^^fn<int>;      // { dg-error "insufficient contextual information to determine type" }

constexpr auto c = ^^std::vector;  // OK

template <typename T>
struct S {
  static constexpr auto r = ^^T;
  using type = T;
};
static_assert(S<int>::r == ^^int);
static_assert(^^S<int>::type != ^^int);

typedef struct X {} Y;
typedef struct Z {} Z;
constexpr auto e = ^^Y;  // OK, represents the type alias Y
constexpr auto f = ^^Z;  // OK, represents the type Z (not the type alias)
