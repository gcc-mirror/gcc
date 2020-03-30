// Verify that top-level cv-qualifiers on parameter types are considered
// when determining the function type of an instantiated function template.
// This resolves a part of Core issues 1001/1322.
// { dg-do compile { target c++11 } }
// { dg-additional-options "-Wno-volatile" }

template<typename... Ts>
void foo0(Ts... t);

template<typename... Ts>
void foo1(const Ts... t);

template<typename... Ts>
void foo2(volatile Ts... t);

template<typename... Ts>
void foo3(const volatile Ts... t);

#if __cplusplus >= 201103L
#define SA(X) static_assert(X,#X)
SA(__is_same(decltype(foo0<char[]>), void(char*)));
SA(__is_same(decltype(foo0<const char[]>), void(const char*)));
SA(__is_same(decltype(foo0<volatile char[]>), void(volatile char*)));
SA(__is_same(decltype(foo0<const volatile char[]>), void(const volatile char*)));

SA(__is_same(decltype(foo1<char[]>), void(const char*)));
SA(__is_same(decltype(foo1<const char[]>), void(const char*)));
SA(__is_same(decltype(foo1<volatile char[]>), void(const volatile char*)));
SA(__is_same(decltype(foo1<const volatile char[]>), void(const volatile char*)));

SA(__is_same(decltype(foo2<char[]>), void(volatile char*)));
SA(__is_same(decltype(foo2<const char[]>), void(const volatile char*)));
SA(__is_same(decltype(foo2<volatile char[]>), void(volatile char*)));
SA(__is_same(decltype(foo2<const volatile char[]>), void(const volatile char*)));

SA(__is_same(decltype(foo3<char[]>), void(const volatile char*)));
SA(__is_same(decltype(foo3<const char[]>), void(const volatile char*)));
SA(__is_same(decltype(foo3<volatile char[]>), void(const volatile char*)));
SA(__is_same(decltype(foo3<const volatile char[]>), void(const volatile char*)));
#endif

int main()
{
  foo0<char[]>(0);
  foo0<const char[]>(0);
  foo0<volatile char[]>(0);
  foo0<const volatile char[]>(0);

  foo1<char[]>(0);
  foo1<const char[]>(0);
  foo1<volatile char[]>(0);
  foo1<const volatile char[]>(0);

  foo2<char[]>(0);
  foo2<const char[]>(0);
  foo2<volatile char[]>(0);
  foo2<const volatile char[]>(0);

  foo3<char[]>(0);
  foo3<const char[]>(0);
  foo3<volatile char[]>(0);
  foo3<const volatile char[]>(0);
}
