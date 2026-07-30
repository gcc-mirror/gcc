/* PR rtl-optimization/126501 */
/* If-conversion emitted a fresh comparison at the end of the test block
   while a condition-code value set earlier in that block was still live
   in the join block.  */

struct F { unsigned int f0 : 2, f1 : 2, f2 : 2; };
struct G { unsigned int g : 2; };
struct S { struct F f; struct G g[6]; };

__attribute__((noipa)) int
f (struct S *p, int x)
{
  p->g[1].g = (1 >= p->f.f1);
  p->g[2].g = x ? p->f.f0 : p->f.f2;
  p->g[0].g = p->g[1].g + p->g[2].g;
  return p->g[0].g;
}

__attribute__((noipa)) int
ref (struct S *p, int x)
{
  volatile int f0 = p->f.f0, f1 = p->f.f1, f2 = p->f.f2;
  volatile int a = (1 >= f1);
  volatile int b = (x ? f0 : f2);

  p->g[1].g = a;
  p->g[2].g = b;
  p->g[0].g = (int) p->g[1].g + (int) p->g[2].g;
  return p->g[0].g;
}

__attribute__((noipa)) int
opaque (int v)
{
  return v;
}

int
main (void)
{
  struct S s;
  int i, j, k, xi;

  for (i = 0; i < 4; i++)
    for (j = 0; j < 4; j++)
      for (k = 0; k < 4; k++)
	for (xi = 0; xi < 2; xi++)
	  {
	    int x = opaque (xi);
	    int got, want;

	    __builtin_memset (&s, 0, sizeof s);
	    s.f.f0 = i; s.f.f1 = j; s.f.f2 = k;
	    got = f (&s, x);

	    __builtin_memset (&s, 0, sizeof s);
	    s.f.f0 = i; s.f.f1 = j; s.f.f2 = k;
	    want = ref (&s, x);

	    if (got != want)
	      __builtin_abort ();
	  }
  return 0;
}
