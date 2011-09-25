// PR c++/39060
// { dg-do compile }

struct A
{
  A(void* i=);	// { dg-error "with|specification|primary-expression" }
  A(void* i=);	// { dg-error "overloaded|primary-expression" }
  A(void* i=);	// { dg-error "overloaded|primary-expression" }

  void operator+ (void* i=);	// { dg-error "arguments" }

  virtual void foo1(=);	// { dg-error "identifier|primary-expression" }
  void foo2(=);		// { dg-error "identifier|primary-expression" }
  void foo3(=);		// { dg-error "identifier|primary-expression" }
  void foo4(=);		// { dg-error "identifier|primary-expression" }
  void foo5(=);		// { dg-error "identifier|primary-expression" }
};

A::A (void* i=) {}	// { dg-error "primary-expression|argument" }
