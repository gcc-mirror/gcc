// PR c++/58811

struct B
{
  struct A a; // { dg-error "incomplete type" }
};

void foo()
{
  B();
}
