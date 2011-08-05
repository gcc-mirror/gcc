// PR c++/49983
// { dg-options -std=c++0x }

template <class T>
void f(T t)
{
  for (auto v : t);
}
