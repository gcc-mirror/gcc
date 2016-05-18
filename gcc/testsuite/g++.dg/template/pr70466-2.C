// PR c++/70466

template < class T, class S >
struct A
{
  explicit A (...) {}
};

template < class T, class S >
A < T, S > foo (T (S::*f) ())
{
  return A < T, S > (f);
}

struct B
{
  void bar () {}
};

int
main ()
{
  foo (&B::bar);
  return 0;
}
