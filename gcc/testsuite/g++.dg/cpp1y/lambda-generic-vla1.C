// PR c++/59271
// { dg-options -std=c++1y }

extern "C" int printf (const char *, ...);

void f(int n)
{
  int  a[n];

  for (auto& i : a)
    {
      i = &i - a;
    }

  [&a] (auto m)
    {
      for (auto i : a)
	{
	  printf ("%d", i);
	}

      return m;
    };
}
