// PR c++/60064
// { dg-do compile }
// { dg-options "-std=c++1y" }

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
