// CWG 2518
// { dg-do compile { target c++11 } }

template <class T> void f()
{
  static_assert (false, "");
}
