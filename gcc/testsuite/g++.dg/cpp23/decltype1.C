// PR c++/101165 - P2266R1 - Simpler implicit move
// { dg-do compile { target c++23 } }
// Tests from P2266R1, decltype-related changes in
// $ 3.2.1. Interaction with decltype and decltype(auto)

template<typename T, typename U>
struct same_type { static const bool value = false; };

template<typename T>
struct same_type<T, T> { static const bool value = true; };

auto f1(int x) -> decltype(x) { return (x); }
static_assert(same_type<decltype(f1), int (int)>::value);
auto f2(int x) -> decltype((x)) { return (x); }   // { dg-error "cannot bind" }
static_assert(same_type<decltype(f2), int& (int)>::value);
auto f3(int x) -> decltype(auto) { return (x); }  // { dg-warning "reference to local variable" }
static_assert(same_type<decltype(f3), int&& (int)>::value);
auto g1(int x) -> decltype(x) { return x; }
static_assert(same_type<decltype(g1), int (int)>::value);
auto g2(int x) -> decltype((x)) { return x; }	  // { dg-error "cannot bind" }
static_assert(same_type<decltype(g2), int& (int)>::value);
auto g3(int x) -> decltype(auto) { return x; }
static_assert(same_type<decltype(g3), int (int)>::value);

// Note that f2 and g2 are well-formed in C++20, but we propose to make
// f2 and g2 ill-formed, because they attempt to bind an lvalue reference
// to a move-eligible xvalue expression.

struct X { };

auto
f4 (X x)
{
  return x;
}
static_assert(same_type<decltype(f4), X(X)>::value);

auto&
f5 (X x)
{
  return x; // { dg-error "cannot bind non-const lvalue reference" }
}
static_assert(same_type<decltype(f5), X&(X)>::value);

auto&&
f6 (X x)
{
  return x; // { dg-warning "reference to local variable" }
}
static_assert(same_type<decltype(f6), X&&(X)>::value);

auto
f7 (X x)
{
  return (x);
}
static_assert(same_type<decltype(f7), X(X)>::value);

auto&
f8 (X x)
{
  return (x); // { dg-error "cannot bind non-const lvalue reference" }
}
static_assert(same_type<decltype(f8), X&(X)>::value);

auto&&
f9 (X x)
{
  return (x); // { dg-warning "reference to local variable" }
}
static_assert(same_type<decltype(f9), X&&(X)>::value);

decltype(auto)
f10 (X x)
{
  return x;
}
static_assert(same_type<decltype(f10), X(X)>::value);

decltype(auto)
f11 (X x)
{
  return (x); // { dg-warning "reference to local variable" }
}
static_assert(same_type<decltype(f11), X&&(X)>::value);

decltype(auto)
f12 (X& x)
{
  return x;
}
static_assert(same_type<decltype(f12), X&(X&)>::value);

decltype(auto)
f13 (X& x)
{
  return (x);
}
static_assert(same_type<decltype(f13), X&(X&)>::value);

decltype(auto)
f14 (X&& x)
{
  return x;
}
static_assert(same_type<decltype(f14), X&&(X&&)>::value);

decltype(auto)
f15 (X&& x)
{
  return (x);
}
static_assert(same_type<decltype(f15), X&&(X&&)>::value);
