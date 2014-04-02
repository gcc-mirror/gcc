// PR c++/39055

struct A
{
  int i;			// { dg-message "" }
  A() { void foo(int=i); }	// { dg-error "" }
};
