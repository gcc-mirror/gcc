// Verify that top-level cv-qualifiers on parameter types are considered
// when determining the function type of an instantiated function template.
// This resolves a part of Core issues 1001/1322.
// { dg-do compile }
// { dg-additional-options "-Wno-volatile" }

template<typename T>
void foo0(T t = 0);

template<typename T>
void foo1(const T = 0);

template<typename T>
void foo2(volatile T t = 0);

template<typename T>
void foo3(const volatile T t = 0);

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
  foo0<char[]>();
  foo0<const char[]>();
  foo0<volatile char[]>();
  foo0<const volatile char[]>();

  foo1<char[]>();
  foo1<const char[]>();
  foo1<volatile char[]>();
  foo1<const volatile char[]>();

  foo2<char[]>();
  foo2<const char[]>();
  foo2<volatile char[]>();
  foo2<const volatile char[]>();

  foo3<char[]>();
  foo3<const char[]>();
  foo3<volatile char[]>();
  foo3<const volatile char[]>();
}
