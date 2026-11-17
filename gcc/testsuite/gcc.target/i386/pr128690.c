/* PR target/128690 */
/* { dg-do compile } */
/* { dg-options "-O3 -msse2 -fno-vect-cost-model" } */

extern void g (int);
extern int h ();
char a, b;
int d, e;

void
f ()
{
  for (int c = 0; c < 8; ++c)
    a = 1;
}

void
i (int j)
{
  char k[8], l[8];
  k[0] = k[1] = k[2] = k[3] = k[4] = k[5] = k[6] = k[7] = 6;
  int *m = 0;
  char *n = 0;
  if (!d)
    {
      *m = 0;
      n = &b;
      goto p;
    }
  do
    {
      if (e)
	goto r;
    p:
      *n = 0;
      for (int s = 0; s < 8; ++s)
	l[s] = k[s];
    }
  while (b);
  l[6] = k[6] - 1;
  l[7] = k[7] - 1;
  if (j - h ())
  r:
    f ();
  k[6] = l[6];
  k[7] = 7 + l[7];
  g (k[6]);
  g (k[7]);
  g (l[7]);
}
