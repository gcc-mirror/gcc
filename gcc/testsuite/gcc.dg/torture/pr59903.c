/* { dg-do compile } */

int a, b, c, d, e, f, g, h, i[3], l, m, n, o, p, q, r;

struct S0
{
  int f0;
  int f1;
  int f2;
  int f3;
} j;

static int
fn1 (int p1)
{
  return p1 || ((p1 > 0) > (e << 1)); 
}

static struct S0
fn2 (struct S0 p1)
{
  char s;
  struct S0 t = {0,0,0,0};
  int u = 2;
  for (;;)
    {
      if (i[0])
	break;
      for (m = 0; m < 4; m++)
	for (p1.f0 = 0; p1.f0 < 3; p1.f0++)
	  {
	    j = t;
	    t.f3 = i[p1.f0];
	    o = b || 1 >> b ? 0 : a < 0;
	    q = 1 % d;
	    if ((g < fn1 ((1 ^ (q & 1)) | n)) ^ u)
	      j.f3 |= p % 2;
	    s = j.f3 > 0 ? j.f3 : j.f3 << 1;
	    r = l = s && p1.f1 * c;
	    h = p1.f1;
	  }
    }
  return p1;
}

int
main ()
{
  for (;f;)
    {
      struct S0 v = {0,0,0,0};
      fn2 (v);
      j.f3 = 0;
    }
  return 0;
}
