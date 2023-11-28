// PR c++/112744
// { dg-do compile { target c++11 } }

struct A { int a = 0; };
struct B : A {};
struct C : A {};
struct D : B, C {};

int main()
{
  D d;
  (void) d.a;	  // { dg-error "request for member .a. is ambiguous" }
  (void) d.A::a;  // { dg-error ".A. is an ambiguous base of .D." }
}
