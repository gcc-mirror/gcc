// PR c++/96082
// { dg-do compile { target c++11 } }

template <class> class A {};

void
f ()
{ 
  typename::template A <int> a;
  ::template A <int> a2;
}
