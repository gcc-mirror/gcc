/* { dg-do compile } */

void foo (int * __restrict x, int *y)
{
  for (int i = 0; i < 1024; ++i)
    {
      x[4*i+0] = y[3*i+0];
      x[4*i+1] = y[3*i+1] * 2;
      x[4*i+2] = y[3*i+2] + 3;
      x[4*i+3] = y[3*i+2] * 2 - 5;
    }
}

/* { dg-final { scan-tree-dump "vectorizing stmts using SLP" "vect" { target { vect_int && vect_int_mult } } } } */
