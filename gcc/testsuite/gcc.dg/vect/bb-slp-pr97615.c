/* { dg-do compile } */

short *a;
int e, f;

void
foo (int c, int d)
{
  short *a1, *a2, *a3;
  a1 = a++;
  *a1 = c;
  a2 = a++;
  *a2 = *a1;
  a3 = a++;
  *a3 = d;
}

void
bar (void)
{
  foo (e + f - 2, e + f - 1);
  foo (e + f - 1, 0);
}
