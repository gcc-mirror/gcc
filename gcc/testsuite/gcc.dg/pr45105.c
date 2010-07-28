/* PR debug/45105 */
/* { dg-do compile } */
/* { dg-options "-Os -fcompare-debug" } */

extern int *baz (int *, int *);

void
bar (int *p1, int *p2)
{
  int n = *baz (0, 0);
  p1[n] = p2[n];
}

void
foo (int *p, int l)
{
  int a1[32];
  int a2[32];
  baz (a1, a2);
  while (l)
    {
      if (l & 1)
	p = baz (a2, p);
      l--;
      bar (a1, a2);
    }
}
