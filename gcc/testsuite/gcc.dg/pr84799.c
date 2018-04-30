/* { dg-do compile } */
/* { dg-options "-O" } */

int a, g;
long b, f;
short c, d, e;

static void fn1 ()
{
  if (a)
    {
      long i = 5, j = b / i | c;
      int k = b % (d % i) & j;
      short l = ~f % (-d / j ^ -e), m = e << (d - l);
      if (k)
        m = d;
      i = d | (i & b);
      g = (c | ~f) % i + l;
      e = (c - f) & e;
      d = m;
    }
}

int main ()
{
  b |= 1;
  fn1 ();
  return 0; 
}
