// Build don't link:
// Origin: Jason Merrill <jason@cygnus.com>
// Special g++ Options: -Wno-non-template-friend

template<class T> struct A
{
  friend void f ();
};

A<short> a;
A<int> b;

