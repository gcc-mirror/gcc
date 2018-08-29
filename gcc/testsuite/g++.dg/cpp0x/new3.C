// PR c++/85847
// { dg-do compile { target c++11 } }

template <class>
int f(int b) { return b; }

template <class>
void g()
{
  auto a = new int[f<int>(2), 2];
}
