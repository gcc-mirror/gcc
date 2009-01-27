// PR c++/37554

struct A {};
class B : A {};

void foo(B b)
{
  (A)b; // { dg-error "inaccessible base" }
}
