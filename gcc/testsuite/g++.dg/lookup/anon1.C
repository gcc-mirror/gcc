// PR c++/2039
// Test that a scoped reference to a member of an anonymous union member of
// a base class works properly.

// { dg-do run }

struct A
{
  long ia1;
  union
  {
    long ia2;
  };
};

struct B : public A
{
  void f1();
  void f2();
};

void B::f1()
{
  ia1 = 11;
  ia2 = 22;
}

void B::f2()
{
  ia1    = 33;
  A::ia2 = 44;   // <<< !!!????
}

int main()
{
  B x;

  x.f1();
  if (x.ia1 != 11 || x.ia2 != 22)
    return 1;

  x.f2();
  if (x.ia1 != 33 || x.ia2 != 44)
    return 1;

  return 0;
}
