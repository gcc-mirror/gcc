// { dg-do compile }

// Origin: <bagnara@cs.unipr.it>

// PR c++/7809: Befriending inaccessible name.

class A {
private:
  void f();		// { dg-error "private" }
};

class B {
  friend void A::f();	// { dg-error "context" }
};
