// Build don't link:
// Origin: Jason Merrill <jason@cygnus.com>

template<class T> struct A
{
  friend void f ();
};

A<short> a;
A<int> b;

