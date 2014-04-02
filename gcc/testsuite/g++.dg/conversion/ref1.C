// PR c++/60345

struct C {};
struct J : C {};
struct A {
  operator J* ();
};
A p;
C* const& q = p;
