// { dg-do compile }
// { dg-options "-fno-exceptions" }

struct S { ~S(); };
extern void bar();

void f0()
{
  S s;
  bar();
}

void f1()
{
  try {} catch (...) {}		// { dg-error "" }
}

void f2()
#if __cplusplus <= 201402L
throw(int)			// { dg-warning "deprecated" "" { target { c++11 && { ! c++1z } } } }
#endif
{
  bar();
}
