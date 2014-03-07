// { dg-do compile { target c++11 } }
template<typename T, int... Dims>
struct array { 
  static const int value = 0;
};

template<>
struct array<int, 17> {
  static const int value = 1;
};

template<int... Dims>
struct array<float, 1, Dims...> {
  static const int value = 2;
};

template<typename T, int... Dims>
struct array<T, 1, Dims...> {
  static const int value = 3;
};

int a0[array<int>::value == 0? 1 : -1];
int a1[array<int, 17>::value == 1? 1 : -1];
int a2[array<float, 1, 2, 3>::value == 2? 1 : -1];
int a3[array<double, 1, 2, 3>::value == 3? 1 : -1];
