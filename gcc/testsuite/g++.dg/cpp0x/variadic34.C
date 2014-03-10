// { dg-do compile { target c++11 } }
template<int I, typename... Args>
void get_ith(const Args&... args);

void f()
{
  get_ith<1>(1, 2, 3);
  get_ith<1, int>(1, 2.0, 'x');
  get_ith<1, int, double>(1, 2.0, 'x');
  get_ith<1, int, double, char>(1, 2.0, 'x');
}
