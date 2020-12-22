// PR c++/98353
// { dg-do compile { target c++11 } }

template <int N> struct A {};
template <typename T>
struct B
{
  static const int n = 1;
  template <class> A <B<T>::n> foo ();
  _Complex double c[2], d = 1.0;
};

void
bar ()
{
  B<int>().foo<int> ();
}
