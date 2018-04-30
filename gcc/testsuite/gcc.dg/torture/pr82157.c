/* { dg-do compile } */

int a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z;

int aa ()
{ 
  w = f < 0 || e >> f;
  while (z)
    h = i && (r && p) | ((l = p) == c % d);
  k = v + 1 < a;
  t = -(j < 1) * q;
  return u;
}

int ab ()
{ 
  for (j = 0; 1; j = 5)
    if (!s)
      return d;
}

void ac ()
{ 
  char ad = aa ();
  ab ();
  if (x)
    { 
      for (m = 0; m < 3; m = a)
	{ 
	  y = a && b;
	  if (g)
	    break;
	}
      n = j;
    }
  o = j & ad;
}

int main ()
{ 
  ac ();
  return 0;
}
