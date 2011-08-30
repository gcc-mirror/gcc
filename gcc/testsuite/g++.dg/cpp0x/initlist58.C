// PR c++/50209
// { dg-options -std=c++0x }

struct S { int i,j; };

struct A
{
  static void f (S = {1,2});
};

void f (S = {3,4});

int main()
{
  A::f();
  f();
}
