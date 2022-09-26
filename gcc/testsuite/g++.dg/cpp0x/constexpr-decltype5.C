// PR c++/99130
// { dg-do compile { target c++11 } }

template<class T>
struct A {
  static constexpr int value = T::nonexistent;
};

using type = const int;
using type = decltype(A<int>::value);

#if __cpp_variable_templates
struct B {
  template<class T>
  static constexpr int value = T::nonexistent;
};

template<class T>
constexpr int value = T::nonexistent;

using type = decltype(B::value<int>);
using type = decltype(value<int>);
#endif
