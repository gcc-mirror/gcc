// { dg-do assemble  }
// Bug: g++ forgets to instantiate A<int>
// Contributed by Jason Merrill <jason@cygnus.com>

template <class T> struct A { };
void f () throw (A<int>);
