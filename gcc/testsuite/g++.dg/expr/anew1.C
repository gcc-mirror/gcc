// { dg-do run }
// PR 11228: array operator new, with zero-initialization and a variable sized array.
// Regression test for PR 
// Author: Matt Austern <austern@apple.com>


int* allocate(int n)
{
  return new int[n]();
}

int main()
{
  const int n = 17;
  int* p = allocate(n);
  for (int i = 0; i < n; ++i)
    if (p[i] != 0)
      return 1;
  return 0;
}
