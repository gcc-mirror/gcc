// { dg-require-effective-target c++11 }

struct A
{
  virtual void f() & = 0;
};

struct B: A
{
  void f();			// doesn't override
};

B b;				// { dg-error "abstract" }
