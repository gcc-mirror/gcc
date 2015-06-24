// { dg-do compile { target c++11 } }
template<typename... Args>
struct tuple {
  static const int value = 0;
};

template<typename T, template<class T> class... Metafunctions>
struct tuple<Metafunctions<T>...> {
  static const int value = 1;
};

template<typename T> struct add_pointer;
template<typename T> struct add_reference;

int a0[tuple<int, float>::value == 0? 1 : -1];
int a1[tuple<add_pointer<int>, add_pointer<float> >::value == 0? 1 : -1];
int a2[tuple<>::value == 0? 1 : -1];
int a3[tuple<add_pointer<int> >::value == 1? 1 : -1];
int a4[tuple<add_pointer<int>, add_reference<int> >::value == 1? 1 : -1];
