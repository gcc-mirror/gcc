// Test that passing a non-variadic template to a variadic TTP works
// with explicit template arguments in a function call..
// { dg-do compile { target c++11 } }

template<template<typename> class Property, typename Type>
bool test_property(typename Property<Type>::value_type value);

template<template<typename...> class Property,
	 typename Type1, typename... Types>
bool test_property(typename Property<Type1, Types...>::value_type value);

template <class T>
struct X
{
  using type = X;
  using value_type = int;
  static const value_type value = 42;
};

int main()
{
  test_property<X,int>(42);	// { dg-error "ambiguous" }
}
