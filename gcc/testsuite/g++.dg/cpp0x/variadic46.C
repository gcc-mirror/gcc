// { dg-options "-std=gnu++0x" }
template<typename... Args>
int& f(Args&...);

template<typename... Args>
float& f(const Args&...);

int& g(int x, float y)
{
  return f(x, y);
}
