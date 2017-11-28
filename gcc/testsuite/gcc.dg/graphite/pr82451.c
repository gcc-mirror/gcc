/* { dg-do compile } */
/* { dg-options "-O -floop-parallelize-all" } */

static int a[];
int b[1];
int c;
static void
d (int *f, int *g)
{
  int e;
  for (e = 0; e < 2; e++)
    g[e] = 1;
  for (e = 0; e < 2; e++)
    g[e] = f[e] + f[e + 1];
}
void
h ()
{
  for (;; c += 8)
    d (&a[c], b);
}
