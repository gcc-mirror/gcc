// PR c++/84441
// { dg-do compile { target c++11 } }

struct B {
  int *b;
};
struct A {
  B b;
  A (A &&);
};
struct C {
  A c;
  int d;
};
C bar ();
struct D : C {
  D ()
    : C (0 ? bar () : bar ())
  {}
};
D d;
