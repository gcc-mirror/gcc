// Bug: g++ forgets to instantiate A<int>
// Contributed by Jason Merrill <jason@cygnus.com>
// Build don't link:

template <class T> struct A { };
void f () throw (A<int>);
