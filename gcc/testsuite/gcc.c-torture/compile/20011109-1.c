typedef struct { short x[4]; } S;
typedef struct { unsigned int a, b, c; S *d; } T;

S *(*foo) (T *, int, int, int, int);
unsigned short *(*bar)(const T *);
unsigned short baz(T *,const int);

T *die (void)
{
  typedef struct { unsigned int a, b, e; double f, g; } U;

  char h[8], i[2053], j[2053];
  double k, l, m;
  U n;
  T *o;
  unsigned short p;
  int q, r;
  long s;
  unsigned short *t;
  S *u;
  unsigned char *v, *w;
  unsigned int x;

  o = 0;
  for (x = 0; x < n.e; x++)
    {
      l = 1.0;
      if (n.g - n.f <= 1.0)
	l = ((1 << o->c) - 1) / (n.g - n.f);
      v = w;
      for (r = o->b - 1; r >= 0; r--)
	{
	  u = foo (o, 0, r, o->a, 1);
	  if (!u)
	    break;
	  t = bar (o);
	  for (q = 0; q < (int) o->a; q++)
	    {
	      h[0] = *v;
	      s = *v++;
	      k = (double) s;
	      m = l*k;
	      p = m < 0 ? 0 : m > (1 << o->c) - 1 ? (1 << o->c) - 1 : m + 0.5;
	      p = baz (o,p);
	      t[q] = p;
	      *u++ = o->d[p];
	    }
	}
    }
  return o;
}
