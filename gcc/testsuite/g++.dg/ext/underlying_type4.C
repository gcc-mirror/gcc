// { dg-do compile }
// { dg-skip-if "requires hosted libstdc++ for tr1/type_traits" { ! hostedlib } }

#include <tr1/type_traits>

using namespace std::tr1;

enum E1 { };
enum E2 { a = -1, b = 1 };
enum E3 { c = __LONG_MAX__ };

typedef __underlying_type(E1) UTE1;
typedef __underlying_type(E2) UTE2;
typedef __underlying_type(E3) UTE3;

template<typename T>
  struct underlying_type
  { typedef __underlying_type(T) type; };

int test1[is_same<underlying_type<E1>::type, UTE1>::value ? 1 : -1];
int test2[is_same<underlying_type<E2>::type, UTE2>::value ? 1 : -1];
int test3[is_same<underlying_type<E3>::type, UTE3>::value ? 1 : -1];

int test4[is_integral<underlying_type<E1>::type>::value ? 1 : -1];
int test5[is_integral<underlying_type<E2>::type>::value ? 1 : -1];
int test6[is_integral<underlying_type<E3>::type>::value ? 1 : -1];
