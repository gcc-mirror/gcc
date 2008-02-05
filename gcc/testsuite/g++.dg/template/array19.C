// PR c++/33553
// { dg-do compile }

template <class T> struct S { static const int sz = 2; };
template <class T> struct U { enum { sz = 2 }; };

template <class R>
struct P
{
  template <class T> void bar (int (&x)[S<T>::sz]);
  template <class T> void baz (int (&x)[U<T>::sz]);
};

P<int> p;

void
foo (void)
{
  int x[2];
  p.bar<int> (x);
  p.baz<int> (x);
}
