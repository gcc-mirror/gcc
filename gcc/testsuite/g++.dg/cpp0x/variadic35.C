// { dg-options "-std=gnu++0x" }
template<int I, typename... Args>
void get_ith(const Args&... args);

void f()
{
  get_ith<1, float>(1, 2.0, 'x');
  get_ith<1, int, double, char, int>(1, 2.0, 'x'); // { dg-error "no matching function" }
}
