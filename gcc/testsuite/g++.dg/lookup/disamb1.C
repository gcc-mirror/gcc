// PR c++/525
// Bug: With -pedantic, we weren't converting this to B1* for the call.

struct A
{
  void f();
};

struct B1: public A {};
struct B2: public A {};

struct C: public B1, public B2
{
  void g() { B1::f(); };
};
