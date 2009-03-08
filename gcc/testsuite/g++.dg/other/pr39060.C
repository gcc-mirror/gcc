// PR c++/39060
// { dg-do compile }

struct A
{
  A(void* i=);	// { dg-error "with|specification" }
  A(void* i=);	// { dg-error "overloaded" }
  A(void* i=);	// { dg-error "overloaded" }

  void operator+ (void* i=);	// { dg-error "arguments" }

  virtual void foo1(=);	// { dg-error "identifier" }
  void foo2(=);		// { dg-error "identifier" }
  void foo3(=);		// { dg-error "identifier" }
  void foo4(=);		// { dg-error "identifier" }
  void foo5(=);		// { dg-error "identifier" }
};	// { dg-error "primary-expression" }

A::A (void* i=) {}	// { dg-error "primary-expression|argument" }
