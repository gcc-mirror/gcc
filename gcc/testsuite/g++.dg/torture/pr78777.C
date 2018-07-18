// PR tree-optimization/78777

void bar (char);
struct C { char b; };
struct H { char d[5]; int e; C *f[4]; int g[10]; };
void baz (C *, unsigned char *, int);
unsigned char j[10];

void
foo (H *o, int p, unsigned char *q, int r, char n, H *b)
{
  for (int m = 0; m < o->e; m++)
    {
      if (o->f[m] || o->g[m])
	continue;
      try
	{
	  baz (o->f[m], j, 5);
	  if (p)
	    baz (o->f[m], q, r);
	}
      catch (int)
	{
	  C a = *o->f[m];
	  if (b)
	    bar (n & a.b);
	}
    }
}
