// { dg-do assemble  }

template <class A> class B {    // { dg-message "note" } 
  A a;                          
 public:
  B(A&aa);			// { dg-message "note" }
  ~B();
};
static B<int> b_int (3);	// { dg-error "no matching function" } 
// { dg-message "candidate" "candidate note" { target *-*-* } 9 }
