// PR c++/23947
// { dg-do compile }

class A {};
class B {};
class C : public A, public B {};
class D : public C {};
void f () throw (D)
{
}
