// Build don't link:

template <class T>
struct S1
{
  template <class U>
  struct S2 {};

  template <class X, class Y, class Z>
  void f(X, Y, Z)
    {
      S2<Z> s2z;
    }

  template <class X, class Z>
  void g(X, Z)
    {
      S2<Z> s2z;
    }
};


void h()
{
  S1<int> si;
  si.g(3, 4);
}
