// PR tree-optimization/108647
// { dg-do compile }

bool a;
int b, c;

inline const bool &
foo (bool &e, const bool &f)
{
  return f < e ? f : e;
}

void
bar (signed char e, bool *f, bool *h, bool *g)
{
  for (;;)
    if (g)
      for (signed char j = 0; j < 6;
	   j += ((f[0] & c ? g[0] : int(0 >= e))
		 ? 0 : foo (g[0], g[0] > h[0]) + 1))
	{
	  a = 0;
	  b = 0;
	}
}
