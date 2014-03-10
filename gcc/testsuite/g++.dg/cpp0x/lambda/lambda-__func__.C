// PR c++/44160
// { dg-do link { target c++11 } }

int main()
{
  const char *p = []() { return __func__; }();
  return p == 0;
}
