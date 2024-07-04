// PR c++/98881

template <typename P> struct X {};

template<template <typename> typename TT>
struct X<TT<int>> {
  template<template <typename> typename UU>
  void f (X<UU<int>>&);
};

template<template<class> class TT> struct Y;
template<template<class> class UU> struct Y { };
