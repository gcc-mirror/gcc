// { dg-options "-std=gnu++0x" }
template<typename... Args>
class tuple {};

void f()
{
  tuple<> x;
  tuple<int> y;
  tuple<int, float> z;
}
