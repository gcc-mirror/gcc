/* { dg-do compile } */

void foo (int * __restrict x, int *y)
{
  x = __builtin_assume_aligned (x, __BIGGEST_ALIGNMENT__);
  y = __builtin_assume_aligned (y, __BIGGEST_ALIGNMENT__);
  for (int i = 0; i < 1024; ++i)
    {
      x[3*i+0] = y[2*i+0] * 7 + 5;
      x[3*i+1] = y[2*i+1] * 2;
      x[3*i+2] = y[2*i+0] + 3;
    }
}

/* { dg-final { scan-tree-dump "vectorizing stmts using SLP" "vect" { target { vect_int && vect_int_mult } } } } */
/* { dg-final { scan-tree-dump "LOAD_LANES" "vect" { target { vect_load_lanes } } } } */
