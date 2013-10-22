// { dg-options "-std=gnu++11" }
template<typename... Args>
int f(const Args&...);

void g()
{
  int (*fp)(const int&, const float&) = &f;
}
