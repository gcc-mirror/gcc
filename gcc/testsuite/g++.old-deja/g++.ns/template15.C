// { dg-do assemble  }
// Origin: Mark Mitchell <mark@codesourcery.com>

namespace X {  template <class T> void f () {}   }
template void X::f<int> ();
