// PR c++/41120
// { dg-options "--param ggc-min-heapsize=0 --param ggc-min-expand=0" }

struct A
{
  A();
};

struct B
{
  A a;
};

B b;
