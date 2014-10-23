/* { dg-do compile } */
/* { dg-options "-O3 -msse" } */

long *a, b;
int c;
void
foo (void)
{
  for (c = 0; c < 64; c++)
    a[c] = b >= 0 ? b : -b;
}
