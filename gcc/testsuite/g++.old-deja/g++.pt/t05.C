// Build don't link: 

template <class A> class B {    // ERROR - candidates
  A a;                          
 public:
  B(A&aa);			// ERROR - near match
  ~B();
};
static B<int> b_int (3);	// ERROR - no matching function
