// PR c++/61151
// { dg-do compile { target c++11 } }

struct B
{
  void foo () {}
};

template <class>
struct A
{
  template <class> void bar ();
  B a;
};

template <class T>
template <class U>
void
A<T>::bar ()
{
  auto f = [this] () { auto g = [=] () { a.foo (); }; g (); };
  f ();
}

int
main ()
{
  A<int> a;
  a.bar <int> ();
}
