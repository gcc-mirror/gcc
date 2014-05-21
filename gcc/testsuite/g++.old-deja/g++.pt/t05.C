// { dg-do assemble  }

template <class A> class B {
  A a;                          
 public:
  B(A&aa);			// { dg-message "note" }
  ~B();
};
static B<int> b_int (3);	// { dg-error "no match|rvalue" }
