// Bug: g++ thinks there is a default conversion from void* to B*.
//      There isn't.
// Build don't link:

struct A {
  operator void* ();
};

struct B { };

void foo (B* bp);

void bar (A& a) {
  foo (a);			// ERROR - 
}
