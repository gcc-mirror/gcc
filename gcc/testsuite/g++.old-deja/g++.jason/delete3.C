// PRMS Id: 6093

class A {
public:
  A();
  ~A();
protected:
  void operator delete(void *);	// ERROR - protected
};

A::~A()
{
}

void foo(A *p)
{
  delete p;			// ERROR - in this context
}
