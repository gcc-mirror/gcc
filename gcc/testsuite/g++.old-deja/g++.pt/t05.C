// { dg-do assemble  }

template <class A> class B {    // { dg-error "" } candidates
  A a;                          
 public:
  B(A&aa);			// { dg-error "" } near match
  ~B();
};
static B<int> b_int (3);	// { dg-error "" } no matching function
