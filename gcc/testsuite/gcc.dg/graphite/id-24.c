/* gcc.dg/tree-ssa/loadpre23.c used to ICE with Graphite.  */

struct {
  int a;
  int large[100];
} x;

int foo(int argc)
{
  int b;
  int c;
  int i;
  int d, e;

  for (i = 0; i < argc; i++)
    {
      e = x.a;
      x.a = 9;
    }
  return d + e;
}

