// { dg-do run }
// PR 11228: array operator new, with zero-initialization and a variable sized array.
// Regression test for PR 
// Author: Matt Austern <austern@apple.com>

struct X
{
  int a;
  double b;
};

X* allocate(int n)
{
  return new X[n]();
}

int main()
{
  const int n = 17;
  X* p = allocate(n);
  for (int i = 0; i < n; ++i)
    if (p[i].a != 0 || p[i].b != 0.0)
      return 1;
  return 0;
}
