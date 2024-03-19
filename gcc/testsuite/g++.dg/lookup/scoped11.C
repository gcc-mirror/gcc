// PR c++/112744
// { dg-do compile }

struct A { const static int a = 0; };
struct B : A {};
struct C : A {};
struct D : B, C {};

int main()
{
  D d;
  (void) d.a;
  (void) d.A::a;
}
