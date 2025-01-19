// PR c++/118454
// { dg-do compile { target c++11 } }
// { dg-additional-options --param=hash-table-verification-limit=1000 }

template<class T> using identity = T;

template<class T, class U0, class... Us> struct dual;

template<class T, class... Ts>
using ty1 = dual<identity<T>, Ts...>;

template<class T, class... Ts>
using ty2 = dual<T, Ts...>;
