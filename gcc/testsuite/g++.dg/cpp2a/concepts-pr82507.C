// { dg-do compile { target c++20 } }
// { dg-additional-options "-fconcepts" }

template<class I>
concept True = true;

template<class T>
concept HasType = requires { typename T::type; };

template<class T>
struct S
{
  void foo() requires HasType<T> && True<typename T::type>;
};

S<int> s;

