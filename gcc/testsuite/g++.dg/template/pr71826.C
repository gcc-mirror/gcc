// PR c++/71826  ICE
// PR c++/15272  Invalid ambiguous
// { dg-do compile }

// 15272, we don't search the dependent base
template <class> struct A { int i; };

// We bind to B::i at parse time
struct B { void i () {} };

template <class T> struct C : A <T>, B
{ 
  void f () { i (); } // here
};

int
main ()
{ 
  C <int> c;
  c.f ();
  return 0;
}
