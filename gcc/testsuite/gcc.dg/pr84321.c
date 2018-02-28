/* { dg-do compile } */
/* { dg-options "-O3 -fwrapv" } */

int c;

void
foo (int *a, int b)
{
  int e;
  if (b == 1)
    return;
  for (e = 0; e < (b & ~7); e += 8)
    ;
  for (++e; e < b;)
    c = a[e];
}
