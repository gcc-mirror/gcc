// PR c++/112744
// { dg-do compile }

struct A { const static int a = 0; };
struct B : A {};
struct C : A {};
struct D : B, C {};

int main()
{
  D d;
  (void) d.x;	  // { dg-error ".struct D. has no member named .x." }
  (void) d.A::x;  // { dg-error ".struct A. has no member named .x." }
}
