// PR c++/50209
// { dg-do compile { target c++11 } }

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
