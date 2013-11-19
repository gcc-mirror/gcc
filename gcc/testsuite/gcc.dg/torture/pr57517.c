/* { dg-do compile } */

int x[1024], y[1024], z[1024], w[1024];
void foo (void)
{
  int i;
  for (i = 1; i < 1024; ++i)
    {
      int a = x[i];
      int b = y[i];
      int c = x[i-1];
      int d = y[i-1];
      if (w[i])
	z[i] = (a + b) + (c + d);
    }
}
