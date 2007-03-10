// { dg-options "-std=gnu++0x" }
template<typename... Args>
struct tuple_base {
  static const int value = 0;
};

template<>
struct tuple_base<int> {
  static const int value = 1;
};

template<>
struct tuple_base<int, float> {
  static const int value = 2;
};

template<>
struct tuple_base<float, int> {
  static const int value = 3;
};

template<typename... Args>
struct int_tuple : tuple_base<int, Args...> { };

template<typename... Args>
struct tuple_int : tuple_base<Args..., int> { };

int a0a[int_tuple<int>::value == 0? 1 : -1];
int a0b[int_tuple<int>::value == 0? 1 : -1];
int a1a[int_tuple<>::value == 1? 1 : -1];
int a1b[tuple_int<>::value == 1? 1 : -1];
int a2[int_tuple<float>::value == 2? 1 : -1];
int a3[tuple_int<float>::value == 3? 1 : -1];
