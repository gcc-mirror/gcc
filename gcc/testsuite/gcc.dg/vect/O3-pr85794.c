/* { dg-do compile } */

int a, b, *c, d;
int *f[6];

void
foo (void)
{
  for (b = 1; b >= 0; b--)
    for (d = 0; d <= 3; d++)
      a |= f[b * 5] != c;
}
