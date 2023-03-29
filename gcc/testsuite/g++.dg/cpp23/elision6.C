// PR c++/101165 - P2266R1 - Simpler implicit move
// { dg-do compile { target c++23 } }
// From [diff.cpp20.expr].

template<typename T, typename U>
struct same_type { static const bool value = false; };

template<typename T>
struct same_type<T, T> { static const bool value = true; };

// In C++23, returns int&&; previously returned int&.
decltype(auto) f(int&& x) { return (x); }
static_assert(same_type<decltype(f), int&& (int&&)>::value);

// This used to work in C++20.
int& g(int&& x) { return x; } // { dg-error "cannot bind non-const lvalue reference" }

template<typename T>
decltype(auto) h(T&& x) { return (x); }
static_assert(same_type<decltype(h(42)), int&&>::value);
