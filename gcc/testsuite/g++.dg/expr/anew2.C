// { dg-do run }
// PR 11228: array operator new, with zero-initialization and a variable sized array.
// Regression test for PR 
// Author: Matt Austern <austern@apple.com>


double* allocate(int n)
{
  return new double[n]();
}

int main()
{
  const int n = 17;
  double* p = allocate(n);
  for (int i = 0; i < n; ++i)
    if (p[i] != 0.0)
      return 1;
  return 0;
}
