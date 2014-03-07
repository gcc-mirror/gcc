// PR c++/48015

template <typename T> T f(T);
template <typename T> void g()
{
  int const c = f (1);
  int i = c - 0;
}
