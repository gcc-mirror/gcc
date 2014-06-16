// PR c++/42219

void foo(const void);		// { dg-error "invalid use of cv-qualified" }

void bar()
{
  void (*pf)() = foo;		// { dg-error "cannot convert" }
}
