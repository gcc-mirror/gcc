// PR c++/42219

void foo(const void);		// { dg-error "incomplete|const" }

void bar()
{
  void (*pf)() = foo;		// { dg-error "cannot convert" }
}
