// PR target/12712
// Origin: Markus Schoder <gccbug@gammarayburst.de>

// This used to segfault on x86 because the reg-stack pass
// created an unreachable basic block by purging an outgoing
// edge, and was not prepared to handle it.

// { dg-do compile }

struct A
{
  ~A();
  float f(float x);
  float g() const {return 0;}
};

void h()
{
  A a, b;
  a.f(b.g() + 1);
}
