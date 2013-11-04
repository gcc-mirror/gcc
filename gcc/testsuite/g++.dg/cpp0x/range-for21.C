// PR c++/49983
// { dg-options -std=c++11 }

template <class T>
void f(T t)
{
  for (auto v : t);
}
