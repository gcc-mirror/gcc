// PR c++/60390
// { dg-do compile { target c++14 } }
// { dg-additional-options "-fconcepts" }

struct A
{
  void foo (auto);
};

class B
{
  int m;
  friend void A::foo (auto);
};

void A::foo (auto i)
{
  B b;
  b.m = i;
}

int main ()
{
  A a;
  a.foo (7);
}
