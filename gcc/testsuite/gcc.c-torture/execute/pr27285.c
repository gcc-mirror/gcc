/* PR tree-optimization/27285 */

extern void abort (void);

struct S { unsigned char a, b, c, d[16]; };

void __attribute__ ((noinline))
foo (struct S *x, struct S *y)
{
  int a, b;
  unsigned char c, *d, *e;

  b = x->b;
  d = x->d;
  e = y->d;
  a = 0;
  while (b)
    {
      if (b >= 8)
	{
	  c = 0xff;
	  b -= 8;
	}
      else
	{
	  c = 0xff << (8 - b);
	  b = 0;
	}

      e[a] = d[a] & c;
      a++;
    }
}

int
main (void)
{
  struct S x = { 0, 25, 0, { 0xaa, 0xbb, 0xcc, 0xdd }};
  struct S y = { 0, 0, 0, { 0 }};

  foo (&x, &y);
  if (x.d[0] != y.d[0] || x.d[1] != y.d[1]
      || x.d[2] != y.d[2] || (x.d[3] & 0x80) != y.d[3])
    abort ();
   return 0;
}
