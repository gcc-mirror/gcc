// PR c++/12160
// { dg-options "-fshow-column" }

struct X {
  virtual void f(int,
		 itn,
		 int);
};

// { dg-error "4:'itn' has not been declared" "" { target *-*-* } 6 }
