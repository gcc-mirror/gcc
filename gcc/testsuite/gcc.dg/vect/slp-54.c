/* { dg-do compile } */

void foo (int * __restrict x, int *y)
{
  x = __builtin_assume_aligned (x, __BIGGEST_ALIGNMENT__);
  y = __builtin_assume_aligned (y, __BIGGEST_ALIGNMENT__);
  for (int i = 0; i < 1024; ++i)
    {
      x[6*i+0] = y[4*i+0] * 7 + 5;
      x[6*i+1] = y[4*i+1] * 2;
      x[6*i+2] = y[4*i+2] + 3;
      x[6*i+3] = y[4*i+3] * 7 + 5;
      x[6*i+4] = y[4*i+0] * 2;
      x[6*i+5] = y[4*i+3] + 3;
    }
}

/* { dg-final { scan-tree-dump "vectorizing stmts using SLP" "vect" { target { vect_int && vect_int_mult } } } } */
