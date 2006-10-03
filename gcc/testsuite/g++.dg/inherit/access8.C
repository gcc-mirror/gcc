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
  A::i;
  A::A1;
};

void
f ()
{
  B b;
  B::A1 a1;
}
