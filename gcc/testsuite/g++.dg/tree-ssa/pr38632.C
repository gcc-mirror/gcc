// { dg-do compile }
// { dg-require-effective-target pthread } 
// { dg-options "-O -ftree-parallelize-loops=2" }

void foo();

void bar(int n, char *p)
{
  try
  {
    foo();
    ++n;
    foo();
    for (int i = 0; i < n-1; ++i)
      p[i] = 0;
  }
  catch (...)
  {
    for (int i = 0; i < n; ++i)
      p[i] = 0;
  }
}
