/* { dg-do compile } */

struct A
{
  __complex__ double c;
};

struct B
{
  A a;
  B(A x) : a(x) {}
  void foo();
};

void bar()
{
  B b = A();
  B(b).foo();
}
