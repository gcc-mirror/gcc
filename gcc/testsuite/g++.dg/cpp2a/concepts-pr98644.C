// PR c++/98644
// { dg-do compile { target c++20 } }

template<class T> concept Signed = bool(T(1)); // { dg-error "reinterpret_cast" }
static_assert(Signed<int*>); // { dg-error "non-constant" }

constexpr bool B = requires { requires bool((char *)1); }; // { dg-error "reinterpret_cast" }
