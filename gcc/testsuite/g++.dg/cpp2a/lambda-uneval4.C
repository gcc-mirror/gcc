// { dg-do link { target c++2a } }

template <class T> T f(T t) { return t; }
using L = decltype([]{ return f(42); });
int main()
{
  return L()();
}
