// Build don't link: 

template <class A> class B {   
  A a;                          
 public:
  B(A&aa);			// ERROR - near match
  ~B();
};  // ERROR - candidates
static B<int> b_int (3);	// ERROR - no matching function
