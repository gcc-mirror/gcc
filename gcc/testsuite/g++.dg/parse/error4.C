// PR c++/12160

struct X { 
  virtual void f(int, 
		 itn,        // { dg-error "declared" }
		 int);       // { dg-error "" }
}; 
