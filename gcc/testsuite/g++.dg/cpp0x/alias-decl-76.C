// PR c++/90679
// { dg-do compile { target c++11 } }

template<class T, class...>
using first_t = T;

template<class T>
struct A;

template<class T>
struct traits;

template<class T>
struct traits<A<first_t<T>>> {
  static constexpr int value = 1;
};

template<class T>
struct traits<A<first_t<const T>>> {
  static constexpr int value = 2;
};

static_assert(traits<A<int>>::value == 1, "");
static_assert(traits<A<const int>>::value == 2, ""); // { dg-bogus "ambiguous" }
