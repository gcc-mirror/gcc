// { dg-options "-std=gnu++0x" }
template<typename Signature>
struct function_traits;

template<typename R, typename... ArgTypes>
struct function_traits<R(ArgTypes...)> {
  typedef R result_type;
};

template<typename R, typename Class, typename... ArgTypes>
struct function_traits<R (Class::*)(ArgTypes...)> {
  typedef R result_type;
};

template<typename R, typename Class, typename... ArgTypes>
struct function_traits<R (Class::*)(ArgTypes...) const> {
  typedef R result_type;
};

template<typename T, typename U>
struct same_type {
  static const bool value = false;
};

template<typename T>
struct same_type<T, T> {
  static const bool value = true;
};

struct X {};

int a0[same_type<function_traits<int (X::*)()>::result_type, int>::value? 1 : -1];
int a1[same_type<function_traits<int (X::*)(float)>::result_type, int>::value? 1 : -1];
int a2[same_type<function_traits<int (X::*)(double, char)>::result_type, int>::value? 1 : -1];
int a3[same_type<function_traits<int (X::*)(double, char) const>::result_type, int>::value? 1 : -1];
