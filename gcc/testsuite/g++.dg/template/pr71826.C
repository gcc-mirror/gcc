// PR c++/71826
// { dg-do compile }

template <class> struct A { int i; };	// { dg-message "note" }
struct B { void i () {} };		// { dg-message "note" }
template <class T> struct C : A <T>, B
{ 
  void f () { i (); }			// { dg-error "is ambiguous" }
};

int
main ()
{ 
  C <int> c;
  c.f ();
  return 0;
}
