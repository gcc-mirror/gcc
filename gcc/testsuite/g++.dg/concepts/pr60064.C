// PR c++/60064
// { dg-do compile { target c++14 } }
// { dg-additional-options "-fconcepts" }

class A
{
  int m;
  friend void foo (auto) {}
  friend void foo2 (auto);
};

void foo2 (auto i)
{
  A a;
  a.m = i;
}

int main ()
{
  foo2 (7);
}
