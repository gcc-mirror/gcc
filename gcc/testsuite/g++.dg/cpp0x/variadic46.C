// { dg-do compile { target c++11 } }
template<typename... Args>
int& f(Args&...);

template<typename... Args>
float& f(const Args&...);

int& g(int x, float y)
{
  return f(x, y);
}
