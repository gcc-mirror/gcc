// { dg-do compile { target c++11 } }
template<typename Signature>
struct function_traits;

template<typename R, typename... ArgTypes>
struct function_traits<R(ArgTypes......)> {	// { dg-warning "omission of ',' before varargs '...' is deprecated" "" { target c++26 } }
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

int a0[same_type<function_traits<int(double, char...)>::result_type, int>::value? 1 : -1];	// { dg-warning "omission of ',' before varargs '...' is deprecated" "" { target c++26 } }
int a1[same_type<function_traits<int(double, char,...)>::result_type, int>::value? 1 : -1];
int a2[same_type<function_traits<int(char,...)>::result_type, int>::value? 1 : -1];
int a3[same_type<function_traits<int(...)>::result_type, int>::value? 1 : -1];
int a4[same_type<function_traits<int(double x, char...)>::result_type, int>::value? 1 : -1];	// { dg-warning "omission of ',' before varargs '...' is deprecated" "" { target c++26 } }
int a5[same_type<function_traits<int(double, char y...)>::result_type, int>::value? 1 : -1];	// { dg-warning "omission of ',' before varargs '...' is deprecated" "" { target c++26 } }
