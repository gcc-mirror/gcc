// Build don't link:

class X;

class A {
public:
  void handlerFn(X*);
};

typedef void (A::*handler) (X*);

class B {
public:
  void setHandler(handler); // ERROR - candidate
};

void f(B* b) {
  b->setHandler(A::handlerFn);	// ERROR - 
};
