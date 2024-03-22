// PR c++/90679
// A version of alias-decl-75.C where the specializations of the
// complex alias template first_t are dependent.
// { dg-do compile { target c++11 } }

template<class T, class...>
using first_t = T;

template<class T>
struct A;

template<class T>
struct traits;

template<class T>
struct traits<A<first_t<T, T&>>> {
  static constexpr int value = 1;
};

template<class T>
struct traits<A<first_t<const T, T&>>> {
  static constexpr int value = 2;
};

static_assert(traits<A<int>>::value == 1, "");
static_assert(traits<A<const int>>::value == 2, ""); // { dg-bogus "ambiguous" }
