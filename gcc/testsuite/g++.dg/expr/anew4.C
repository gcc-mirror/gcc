// { dg-do run }
// PR 11228: array operator new, with zero-initialization and a variable sized array.
// Regression test for PR 
// Author: Matt Austern <austern@apple.com>

struct B
{
  B();
  int n;
};

B::B()
{
  n = 137;
}


struct D : public B
{
  double x;
};


D* allocate(int n)
{
  return new D[n]();
}

int main()
{
  const int n = 17;
  D* p = allocate(n);
  for (int i = 0; i < n; ++i)
    if (p[i].n != 137 || p[i].x != 0)
      return 1;
  return 0;
}
