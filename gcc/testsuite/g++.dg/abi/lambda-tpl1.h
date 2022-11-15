inline auto l_auto = [] (auto) {};

inline auto l_tpl = [] <typename T> (T) {};

inline auto l_tpl_auto = [] <typename T> (T, auto) {};

inline auto l_tpl_nt_ary = [] <int I> (int (&)[I]) {};

inline auto l_tpl_nt_auto = [] <auto I = 0> () {};

template<typename T, unsigned I> class U;

template<template<typename, unsigned> typename> class TPL {};
inline auto l_tpl_tpl = [] <template<typename, unsigned> typename T> (TPL<T> &) {};

template<template<template<typename, unsigned> typename> typename> class TPLTPL {};
inline auto l_tpl_tpl_tpl = []<template<template<typename, unsigned> typename> typename T> (TPLTPL<T> &) {};

inline auto l_var = []<typename... Args> (Args...) {};

#if FIXME // we fail to parse (&...) correctly
inline auto l_var2 = []<int... I> (int (&...)[I]) {};
#endif

template<int...I> class X {};
inline auto l_var3 = []<template<int...> typename T, int...I> (T<I...> &a) {};

template<template<typename, unsigned> typename...T> class Y{};
inline auto l_var4 = []<template<typename, unsigned> typename... T> (Y<T...> &a) {};

template<int I> inline void Fn ()
{
  auto l = []<typename T> (T) {};
  l (1);
}

void f () 
{
  l_auto (1);
  l_tpl (1);
  l_tpl_auto (1, 1);
  int ary[2];
  l_tpl_nt_ary (ary);
  l_tpl_nt_auto ();
  TPL<U> v;
  l_tpl_tpl (v);
  TPLTPL<TPL> u;
  l_tpl_tpl_tpl (u);
  l_var (1, 2, 3);
#if FIXME
  l_var2 (ary, ary);
#endif
  X<1,2,3> x;
  l_var3 (x);
  Y<U,U> y;
  l_var4 (y);

  Fn<1> ();

  auto l1 = []<typename T, T v = T(0)> (T a) {
    auto l2 = []<typename U> (T a, U b) {};

    l2 (a, v);
  };
  auto l3 = []<typename T>(U<T, 0> *, U<int, 0> *) {};

  l1 (1);
  l1 ('1');
  l3 ((U<char, 0> *)nullptr, nullptr);
}
