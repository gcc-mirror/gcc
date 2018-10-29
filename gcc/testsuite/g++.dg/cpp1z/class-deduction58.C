// PR c++/86098
// { dg-do compile { target c++17 } }

template <class _Res> class future;
template <class T> T&& declval();

template<template <class...> class T>
struct construct_deduced {
  template <class... AN>
  using deduced_t = decltype(T{declval<AN>()...});
  template<class... AN>
  deduced_t<AN...> operator()(AN&&... an) const;
};

template<class T>
future<T> future_from(T singleSender);
