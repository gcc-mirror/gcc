/* PR target/91408 */
/* { dg-do compile } */
/* { dg-options "-O2 -ftree-loop-vectorize -fno-tree-forwprop" } */

long long a;
unsigned char b;
short *c;
int d;

void
foo (long long *x)
{
  unsigned char *e = (char *) x;
  int f, g = 0;
  for (d = 0; d < 8; d++)
    {
      f = b - e[d];
      if (f < 0)
	f = -f;
      g += f;
    }
  c[0] = g;
}

void
bar (void)
{
  foo (&a);
}
