// PR c++/65882
// { dg-do compile { target c++11 } }
// { dg-options "-Wbool-compare" }

// Check that we don't ICE because of reentering error reporting routines while
// evaluating template parameters

template<typename>
struct type_function {
  static constexpr bool value = false;
};

template<bool>
struct dependent_type {
  typedef int type;
};

template<typename T>
typename dependent_type<(5 > type_function<T>::value)>::type
bar();

template<typename T>
typename dependent_type<(5 > type_function<T>::value)>::type
foo()
{
  return bar<int>();
}

int main()
{
  foo<int>();
}
