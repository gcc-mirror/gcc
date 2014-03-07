// { dg-do compile { target c++11 } }
template<typename... Args>
class tuple {};

void f()
{
  tuple<> x;
  tuple<int> y;
  tuple<int, float> z;
}
