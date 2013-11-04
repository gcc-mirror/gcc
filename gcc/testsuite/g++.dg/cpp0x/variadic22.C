// { dg-options "-std=gnu++11" }
template<typename R, typename... ArgTypes>
struct make_function_type
{
  typedef R type(ArgTypes... args);
};

template<typename T, typename U>
struct is_same {
  static const bool value = false;
};

template<typename T>
struct is_same<T, T> {
  static const bool value = true;
};

int a0[is_same<make_function_type<int>::type, int()>::value? 1 : -1];
int a1[is_same<make_function_type<int, float>::type, int(float)>::value? 1 : -1];
int a2[is_same<make_function_type<int, float>::type, int(float)>::value? 1 : -1];
int a3[is_same<make_function_type<int, float, double>::type, int(float, double const)>::value? 1 : -1];
