/* { dg-do compile } */

int a, b;
int i(int *);
int *c(int *d, int *e)
{
  for (; b;)
    d = e;
  return d;
}
void f()
{
  for (;;)
    {
      int *g[1];
      int h = 0;
      for (; h < 3; h++)
	g[0] = &a;
      &a == g[0] || i(c((int *)g, g[0]));
    }
}
