// PR c++/23947
// { dg-do compile }

class A {};
class B {};
class C : public A, public B {};
class D : public C {};
void f ()
#if __cplusplus <= 201402L
throw (D)			// { dg-warning "deprecated" "" { target { c++11 && { ! c++1z } } } }
#endif
{
}
