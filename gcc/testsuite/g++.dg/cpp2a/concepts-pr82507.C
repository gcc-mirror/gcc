// { dg-do compile { target c++2a } }
// { dg-additional-options "-fconcepts-ts" }

template<class I>
concept bool True = true;

template<class T>
concept bool HasType = requires { typename T::type; };

template<class T>
struct S
{
  void foo() requires HasType<T> && True<typename T::type>;
};

S<int> s;

