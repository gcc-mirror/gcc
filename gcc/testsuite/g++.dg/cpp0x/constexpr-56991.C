// PR c++/56991
// { dg-do compile { target c++11 } }

#include <initializer_list>

constexpr std::initializer_list<int> good1 = { 1, 2, 3 };
struct foo { int a, b; };
constexpr foo good2 = { 1, 2 };

constexpr std::initializer_list<foo> bad1 = { { 1, 2 }, { 3, 4} };
constexpr std::initializer_list<foo> bad2 = { good2, good2 };
