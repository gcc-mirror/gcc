// { dg-do compile { target c++11 } }
template<typename...> struct count;

template<>
struct count<> {
  static const int value = 0;
};

template<typename T, typename... Args>
struct count<T, Args...> {
  static const int value = 1 + count<Args...>::value;
};

int a0[count<>::value == 0? 1 : -1];
int a1[count<char>::value == 1? 1 : -1];
int a2[count<char, short>::value == 2? 1 : -1];
int a3[count<char, short, int>::value == 3? 1 : -1];
int a4[count<char, short, int, long>::value == 4? 1 : -1];
