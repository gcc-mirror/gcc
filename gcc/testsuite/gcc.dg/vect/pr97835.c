/* { dg-do compile } */
/* { dg-require-effective-target vect_int } */

struct co {
  int gx;
  int ty;
};

void
x0 (struct co *yy, long int kc, int wi, int md)
{
  while (wi < 1)
    {
      yy[wi].gx = md;
      yy[wi].ty = wi;
      md += kc;
      ++wi;
    }
}

/* We don't yet support SLP inductions for variable length vectors.  */
/* { dg-final { scan-tree-dump "vectorizing stmts using SLP" "vect" { xfail vect_variable_length } } } */
