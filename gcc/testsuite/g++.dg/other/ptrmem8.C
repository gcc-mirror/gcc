// PR c++/33844
// { dg-do compile }

struct A {};

template<int> void foo(void (A::* f)())
{
  A a;
  &(a.*f);	// { dg-error "3:invalid use of\[^\n\]*\\.\\*\[^\n\]*to form|qualified-id is required" }
}

template<int> void bar(void (A::* f)())
{
  A *p;
  &(p->*f);	// { dg-error "3:invalid use of\[^\n\]*->\\*\[^\n\]*to form|qualified-id is required" }
}
