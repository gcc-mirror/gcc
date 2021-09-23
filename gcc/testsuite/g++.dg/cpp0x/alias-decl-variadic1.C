// PR c++/99445
// { dg-do compile { target c++11 } }
// { dg-additional-options "-fchecking=2 --param=hash-table-verification-limit=1000" }

template <class> struct implicit_conversions;
template <class T>
using implicit_conversions_t = typename implicit_conversions<T>::type;
template <class...> struct response_type;

template <class Handle, class... Ts>
using type1 = response_type<implicit_conversions_t<Ts>...>;

template <class Handle, class... Ts>
using type2 = response_type<typename implicit_conversions<Ts>::type...>;
