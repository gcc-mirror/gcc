// Bug: g++ thinks there is a default conversion from A& to B*.
//      There isn't.
// Build don't link:

struct A {
  operator A* ();
};

struct B: public A { };

void foo (B* bp);

void bar (A& a) {
  foo (a);			// ERROR - 
}
