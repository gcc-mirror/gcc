// PR c++/49092

struct A
{
  A();
};

int i;

A::A()
{
  const int j = i;
}
