// PR c++/112744
// { dg-do compile }

class A { const static int a = 0; };
struct B : A {};
struct C : A {};
struct D : B, C {};

int main()
{
  D d;
  (void) d.a;	  // { dg-error "private" }
  (void) d.A::a;  // { dg-error "private" }
}
