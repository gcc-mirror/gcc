// { dg-do compile { target c++11 } }
template<typename... Args>
struct tuple {
  static const int value = 0;
};

template<>
struct tuple<> {
  static const int value = 1;
};

template<>
struct tuple<int> {
  static const int value = 2;
};


template<>
struct tuple<int, float> {
  static const int value = 3;
};

template<typename T>
struct tuple<T, T> {
  static const int value = 4;
};

template<>
struct tuple<float, float> {
  static const int value = 5;
};

int a0[tuple<float>::value == 0? 1 : -1];
int a1[tuple<>::value == 1? 1 : -1];
int a2[tuple<int>::value == 2? 1 : -1];
int a3[tuple<int, float>::value == 3? 1 : -1];
int a4[tuple<int, int>::value == 4? 1 : -1];
int a5[tuple<float, float>::value == 5? 1 : -1];
