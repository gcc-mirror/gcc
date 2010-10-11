/* PR target/45870 */
/* { dg-do compile } */
/* { dg-options "-g -O" } */
/* { dg-require-effective-target tls } */

__thread int v[30];
int bar (void);

int
foo (int x, int y, int z)
{
  int a, b = z, c;
  while (b > 0)
    {
      c = (bar () % 3);
      a = v[x];
      if (x < y)
	for (;;);
      b += a;
    }
}
