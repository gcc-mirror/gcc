// { dg-do compile { target c++11 } }

struct A			// { dg-error "non-static data member" }
{
  int i = (A(), 42);		// { dg-message "required here" }
};

A a;
