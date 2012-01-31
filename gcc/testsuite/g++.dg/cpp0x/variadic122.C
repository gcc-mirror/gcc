// PR c++/52043
// { dg-options "-std=c++11 -Wreturn-type" }

template < class T > struct Container
{
  T f ();
};

template < class T >
T deref (T)
{}				// { dg-warning "no return" }

template < class T, class ... Args >
auto deref (T u, int, Args ... args)->decltype (deref (u.f (), args ...))
{}				// { dg-warning "no return" }

void
foo ()
{
  Container < Container < int > > v;
  deref (v, 2);
}
