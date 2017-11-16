/* { dg-do compile } */

int printf (const char *, ...);

int b, c, d, e, f, g, j, k;
char h, i;
volatile int l;

int m (int n, int o)
{ 
  return o < 0 || o > 1 ? n : o;
}

int p (int n, unsigned o)
{ 
  return n - o;
}

int q ()
{ 
  char r;
  int a, s, t, u, v, w;
L:
  if (t)
    printf ("%d", d);
  u = v;
  while (j)
    { 
      while (e)
	for (w = 0; w != 54; w += 6)
	  { 
	    l;
	    s = p (u < 1, i || c);
	    r = s < 0 || s > 1 ? 0 : 1 >> s;
	    v = r;
	    g = h;
	  }
      if (h)
	return f;
      if (u)
	for (a = 0; a != 54; a += 6)
	  f = m (2, -(k || b));
    }
  d = t;
  goto L;
}
