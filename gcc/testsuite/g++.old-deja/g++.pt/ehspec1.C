// { dg-do assemble { target c++14_down } }
// Bug: g++ forgets to instantiate A<int>
// Contributed by Jason Merrill <jason@cygnus.com>

template <class T> struct A { };
void f () throw (A<int>);	// { dg-warning "deprecated" "" { target c++11 } }
