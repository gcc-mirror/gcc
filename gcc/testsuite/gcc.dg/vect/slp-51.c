/* { dg-do compile } */

void foo (int * __restrict x, int *y)
{
  x = __builtin_assume_aligned (x, __BIGGEST_ALIGNMENT__);
  y = __builtin_assume_aligned (y, __BIGGEST_ALIGNMENT__);
  for (int i = 0; i < 1024; ++i)
    {
      x[4*i+0] = y[4*i+0];
      x[4*i+1] = y[4*i+2] * 2;
      x[4*i+2] = y[4*i+0] + 3;
      x[4*i+3] = y[4*i+2] * 2 - 5;
    }
}

/* Check we can handle SLP with gaps and an interleaving scheme.  */
/* { dg-final { scan-tree-dump "vectorizing stmts using SLP" "vect" { target { vect_int && vect_int_mult } } } } */
