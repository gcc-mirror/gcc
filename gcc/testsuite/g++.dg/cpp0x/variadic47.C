// { dg-do compile { target c++11 } }
template<typename T> struct wrap { };

template<typename... Args>
int& f(const Args&...);

template<typename... Args>
float& f(const wrap<Args>&...);

int& g(int x, float y, double z)
{
  return f(x, y, z);
}

float& h(wrap<int> x, wrap<float> y, wrap<double> z)
{
  return f(x, y, z);
}
