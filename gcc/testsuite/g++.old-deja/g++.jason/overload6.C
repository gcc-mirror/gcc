// { dg-do assemble  }
// Bug: g++ thinks there is a default conversion from void* to B*.
//      There isn't.

struct A {
  operator void* ();
};

struct B { };

void foo (B* bp);

void bar (A& a) {
  foo (a);			// { dg-error "" } 
}
