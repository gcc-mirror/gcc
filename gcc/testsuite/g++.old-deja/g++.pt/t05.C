// Build don't link: 

template <class A> class B {
  A a;
 public:
  B(A&aa);			// ERROR - 
  ~B();
};
static B<int> b_int (3);	// ERROR - 
