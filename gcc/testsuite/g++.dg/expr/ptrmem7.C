// PR c++/22545

struct A {
  int member;
  A() : member(13) {}
};

A a;

struct B {
  operator A*() { return &a; }
};

B b;

int A::* member_pntr = &A::member;

int main()
{
  return b ->* member_pntr;
}
