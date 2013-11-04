// { dg-options "-std=gnu++11" }
template<typename... Args>
struct tuple_base {
  static const int value = 0;
};

template<>
struct tuple_base<int*>
{
  static const int value = 1;
};

template<typename T>
struct tuple_base<T*>
{
  static const int value = 2;
};

template<typename... Args>
struct tuple_of_pointers : tuple_base<Args*...> { };

int a1[tuple_of_pointers<int>::value == 1? 1 : -1];
int a2[tuple_of_pointers<float>::value == 2? 1 : -1];
