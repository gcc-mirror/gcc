// PR c++/9167
// { dg-options "-finline" }

struct A {
  ~A();
  A f(A) { }
};


void f(void)
{
  A a;
  a.f(a);
}
