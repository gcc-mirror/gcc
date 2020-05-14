// { dg-do compile { target c++20 } }

template<typename...> using A = auto; // { dg-error "not allowed" }
// In pre-20, the error is "invalid use of auto"

template<typename... T> using B = A<T...>;
