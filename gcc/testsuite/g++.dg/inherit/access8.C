// PR c++/29138

class A
{
public:
  int i;
  class A1
  {
    int j;
  };
};

class B : private A
{
public:
  A::i; // { dg-warning "deprecated" }
  A::A1; // { dg-warning "deprecated" }
};

void
f ()
{
  B b;
  B::A1 a1;
}
