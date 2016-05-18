// PR c++/70466

template < class T, class T >  // { dg-error "conflicting" }
class A
{
public:
  explicit A (T (S::*f) ()) {}  // { dg-error "expected" }
};

template < class T, class S > 
A < T, S > foo (T (S::*f) ())
{
  return A < T, S > (f);
}

class B
{
public:
  void bar () {}
};

int
main ()
{
  foo (&B::bar);
  return 0;
}
