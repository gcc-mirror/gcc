// Test that we only call f once and that pointers to different subobjects
// compare as different.

struct A { void f() { } };
struct B: public A { };
struct C: public A { };
struct D : public B, public C { };

typedef void (B::*bp)();
typedef void (C::*cp)();
typedef void (D::*dp)();

dp d1;

int call;

dp f () { ++call; return d1; }

int main()
{
  bp b = &A::f;
  cp c = &A::f;
  d1 = b;
  dp d2 = c;
  return (f() == d2 || call != 1);
}
