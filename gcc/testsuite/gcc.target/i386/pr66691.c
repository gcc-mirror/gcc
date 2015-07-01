/* PR target/66691 */
/* { dg-do compile } */
/* { dg-require-effective-target ia32 } */
/* { dg-options "-O3 -g -mtune=generic -march=i686" } */

unsigned int a;
int b[2], c, d, e, f, g, h, i, k[8], l, m, s, t, w;
static int j;

void
fn1 (long long p)
{
  int t = p;
  c = c ^ b[c ^ (t & 1)];
}

static void
fn2 (long long p)
{
  c = c ^ b[1 ^ (d & 1)];
  fn1 (p >> 1 & 1);
  fn1 (p >> 2);
}

static void
fn3 ()
{
  unsigned char p;
  f = g = 0;
  for (h = 0; h < 6; h++)
    {
      for (s = 0; s < 7; s++)
	if (k[s+1])
	  g = 0;
	else
	  for (j = 0; j < 2; j++)
	    ;
      t = j > 2 ? 0 : 1 >> j;
    }
  if (l)
    {
      short q[2];
      q[0] = q[1] = 0;
      if (m)
	for (i = 0; i < 2; i++)
	  {
	    unsigned char r = q[i];
	    p = f ? r % f : r;
	    e = ((p > 0) <= (q[i] ^ 1)) + a;
	    if (k[1])
	      for (e = 0; e != 18; ++e)
		k[0] = 0;
	  }
    }
}

int
main ()
{
  fn3 ();
  fn2 (w);
  fn2 (j);
  return 0;
}
