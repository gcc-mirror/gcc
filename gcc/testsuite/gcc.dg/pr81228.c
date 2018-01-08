/* PR target/81228.  */
/* { dg-do compile } */
/* { dg-options "-O3 -fdump-tree-ssa" } */

void *a;

void b ()
{
  char c;
  long d;
  char *e = a;
  for (; d; d++)
  {
    double f, g;
    c = g < f || g > f;
    e[d] = c;
  }
}

/* Let's make sure we do have a LTGT.  */
/* { dg-final { scan-tree-dump "<>" "ssa" } } */
