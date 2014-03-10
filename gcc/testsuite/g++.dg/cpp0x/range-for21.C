// PR c++/49983
// { dg-do compile { target c++11 } }

template <class T>
void f(T t)
{
  for (auto v : t);
}
