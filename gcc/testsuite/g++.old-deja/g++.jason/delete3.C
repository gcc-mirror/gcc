// { dg-do assemble  }
// PRMS Id: 6093

class A {
public:
  A();
  ~A();
protected:
  void operator delete(void *);	// { dg-message "" } protected
};

A::~A()
{
}

void foo(A *p)
{
  delete p;			// { dg-error "" } in this context
}
