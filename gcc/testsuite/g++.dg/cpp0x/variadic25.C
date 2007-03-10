// { dg-options "-std=gnu++0x" }
template<int... Values>
struct sum;

template<>
struct sum<> {
  static const int value = 0;
};

template<int Value, int... Values>
struct sum<Value, Values...> {
  static const int value = Value + sum<Values...>::value;
};

int a0[sum<>::value == 0? 1 : -1];
int a1[sum<1, 2, 3, 4, 5>::value == 15? 1 : -1];
