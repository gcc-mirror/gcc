// { dg-do assemble  }
// Bug: g++ thinks there is a default conversion from A& to B*.
//      There isn't.

struct A {
  operator A* ();
};

struct B: public A { };

void foo (B* bp);

void bar (A& a) {
  foo (a);			// { dg-error "" } 
}
