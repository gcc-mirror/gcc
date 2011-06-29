// PR c++/44160
// { dg-options -std=c++0x }
// { dg-do link }

int main()
{
  const char *p = []() { return __func__; }();
  return p == 0;
}
