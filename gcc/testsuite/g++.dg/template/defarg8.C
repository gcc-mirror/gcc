// PR c++/27094
// { dg-options "--param ggc-min-expand=0 --param ggc-min-heapsize=0" }

struct A
{
  ~A();
};

struct B : A
{
  B();
};

template<int> struct C
{
  C(const B& = B());
};

C<0> c;
