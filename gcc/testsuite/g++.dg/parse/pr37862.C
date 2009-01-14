// { dg-do run }
#include <stdlib.h>

class A {
public:
  virtual void get (void) { }
};

class B : public A {
public:
  void get (void) { abort (); }
};

class C : public B { };

int main (void)
{
  C   c;
  C * p = &c;

  p->A::get ();	
  (p->A::get) ();	// The C++ parser used to resolve this to B::get()

  return 0;
}
