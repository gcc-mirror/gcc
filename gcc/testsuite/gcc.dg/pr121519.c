/* PR tree-optimization/121519 */
/* { dg-do compile { target int32plus } } */
/* { dg-options "-O3" } */

extern int foo (void);
int a, b, c;

int
bar (int f)
{
  int d = 0;
  for (; d < 6; d++)
    {
      a = f <<= 1;
      if (f & 64)
	f ^= 67;
    }
  return a;
}

void
baz (void)
{
  int i = 0;
  if (c)
    goto j;
  i = -32644994;
k:
  b = 0;
j:
  if (foo () - 508050053 + bar (i + 79))
    goto k;
}

int
main ()
{
  while (a)
    baz ();
  return 0;
}
