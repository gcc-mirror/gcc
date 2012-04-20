// { dg-do compile }

int g, *gp[100];
struct V {
    int* x;
    int y;
};

void foo (V **p, V* end, int i)
{
  *p = 0;
  V* pp = *p;
  int s = 100;
  for (; pp < end; )
    {
      pp++;
      (pp-1)->x = &g;
      if (g)
	{
	  if (g>10)
	    g++;
	  int *t = (int*) operator new (100);
	  (pp-1)->x = t;
	}
      else
	s--;
      gp[end-pp] = (pp-1)->x + s;
    }
}
