// Bug: g++ thinks there is a conversion from void * to B *.
// Build don't link:

struct A {
  operator void* ();
};

struct B: public A { };

void bar (A& a) {
  B* bp = (B*)a;		// ERROR - 
}
