// { dg-do assemble  }

template <class A> class B {    // { dg-message "note" } 
  A a;                          
 public:
  B(A&aa);			// { dg-message "candidates" }
  ~B();
};
static B<int> b_int (3);	// { dg-error "no matching function" } 
