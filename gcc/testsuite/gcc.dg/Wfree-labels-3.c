/* { dg-do compile } */
/* { dg-options "-Wc11-c23-compat -Wno-free-labels" } */

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
