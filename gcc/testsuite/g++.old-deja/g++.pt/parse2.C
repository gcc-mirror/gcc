// { dg-do assemble  }
// Origin: Jason Merrill <jason@cygnus.com>

template <class T> struct A {
  A (const A&) { }
};

template A<int>::A (const A&);
