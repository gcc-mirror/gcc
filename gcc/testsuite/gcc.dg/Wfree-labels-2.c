/* { dg-do compile } */
/* { dg-options "-std=c11 -pedantic -Wno-free-labels" } */

void
f (void)
{
  goto l;
 l:
}

int
g (void)
{
  goto l;
 l:
  int x = 0;
  return x;
}
