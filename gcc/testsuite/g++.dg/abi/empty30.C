// PR c++/94175
// { dg-do link }

struct A {};
extern A a;

int i;
__attribute ((noinline, noclone))
void f(A) { ++i; }

int main()
{
  f(a);
}
