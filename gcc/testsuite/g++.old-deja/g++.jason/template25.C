// PRMS Id: 6393
// Bug: g++ is too lax in considering UPTs to be the same.

template <class R, class T>
class Bar
{
public:
  R do_bar (T arg);
};


template <class T>
class Foo
{
  T i;

public:
  void do_foo () {}
  void do_foo (T const & t) {}
  void do_foo (Bar<char, T> const & bar);  // {} Put the body here and it works
  void do_foo (Bar<T, T> const & bar);     // {} Put the body here and it works
};

// These definitions don't work

template <class T>
inline void Foo<T>::
do_foo (Bar<char, T> const & bar)
{}

template <class T>
inline void Foo<T>::
do_foo (Bar<T, T> const & bar)
{}


int main ()
{ int i;
  Bar<char, int> bar1;
  Bar<int, int>  bar2;
  Foo<int> foo;
  foo.do_foo();
  foo.do_foo(i);
  foo.do_foo(bar1);
  foo.do_foo(bar2);

  return 0;
}
