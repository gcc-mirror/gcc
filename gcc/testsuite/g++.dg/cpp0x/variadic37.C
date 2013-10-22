// { dg-options "-std=gnu++11" }
template<typename... Values>
struct tuple
{
  static const __SIZE_TYPE__ length = sizeof...(Values);
};

int a0[tuple<>::length == 0? 1 : -1];
int a1[tuple<int>::length == 1? 1 : -1];
int a2[tuple<int, float>::length == 2? 1 : -1];
