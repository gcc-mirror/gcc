// PR c++/60185

template<int> struct A
{
  int i;			// { dg-message "" }
  A() { void foo(int=i); }	// { dg-error "" }
};

A<0> a;
