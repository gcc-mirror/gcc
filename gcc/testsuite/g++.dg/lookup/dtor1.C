// PR c++/12333

struct A { };

struct X { 
  void f () {
    X::~X ();
    this->~X();
    ~X();			// { dg-error "" "unary ~" }
    A::~A ();			// { dg-error "" }
    X::~A ();  			// { dg-error "" }
  }
};
