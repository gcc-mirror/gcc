/* PR rtl-optimization/101260 */
/* { dg-do run { target s390_useable_hw } } */
/* { dg-options "-O1 -march=z14" } */

/* This used to fail with commit: 5dc1390b41d */

struct a
{
  unsigned b:7;
  int c;
  int d;
  short e;
} p, *q = &p;
int f, g, h, i, r, s;
static short j[8][1][6] = { };

char k[7];
short l, m;
int *n;
int **o = &n;
void
t ()
{
  for (; f;)
    ;
}

static struct a
u (int x)
{
  struct a a = { 4, 8, 5, 4 };
  for (; i <= 6; i++)
    {
      struct a v = { };
      for (; l; l++)
	h = 0;
      for (; h >= 0; h--)
	{
	  j[i];
	  struct a *w = &p;
	  s = 0;
	  for (; s < 3; s++)
	    {
	      r ^= x;
	      m = j[i][g][h] == (k[g] = g);
	      *w = v;
	    }
	  r = 2;
	  for (; r; r--)
	    *o = &r;
	}
    }
  t ();
  return a;
}

int
main ()
{
  *q = u (636);
  if (p.b != 4)
    __builtin_abort ();
}
