// PR c++/39055

struct A
{
  int i;
  A() { void foo(int=i); }	// { dg-error "this" }
};
