// PR c++/52008
// { dg-do compile { target c++11 } }

template <int I, typename T, typename... Ts>
struct A;

template<typename... Ts>
struct A<0, Ts...>;		// { dg-error "not more specialized" }
