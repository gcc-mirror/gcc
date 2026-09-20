/* { dg-do run } */
/* { dg-options "-O2" } */

/* The shift is hoisted with its count masked; a count out of range on an
   iteration that never reaches the shift must not affect the result.  */

__attribute__ ((noipa)) void
k (int *p, int n, int a, unsigned int s, int c, int *q)
{
  for (int i = 0; i < n; i++)
    if (p[i] > c)
      {
	if (s > 7) __builtin_unreachable ();
	q[i] = ((a << s) * 5) / 7;
      }
}

__attribute__ ((noipa)) void
g (int *p, int n, int a, unsigned int s, int c, int *q)
{
  for (int i = 0; i < n; i++)
    if (p[i] > c)
      q[i] = (a << s) * 5;
}

int
main (void)
{
  int p[4] = { 1, 5, 2, 9 };
  int q[4] = { -1, -1, -1, -1 };
  k (p, 4, 3, 40u, 100, q);
  g (p, 4, 3, 77u, 100, q);
  for (int i = 0; i < 4; i++)
    if (q[i] != -1)
      __builtin_abort ();
  k (p, 4, 3, 2u, 4, q);
  if (q[0] != -1 || q[1] != (3 << 2) * 5 / 7 || q[2] != -1
      || q[3] != (3 << 2) * 5 / 7)
    __builtin_abort ();
  g (p, 4, 3, 3u, 4, q);
  if (q[1] != (3 << 3) * 5 || q[3] != (3 << 3) * 5)
    __builtin_abort ();
  return 0;
}
