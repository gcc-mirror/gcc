// { dg-options "-std=gnu++0x" }
template<typename... Args>
int f(const Args&...);

void g()
{
  int (*fp)(const int&, const float&) = &f;
}
