/* { dg-do compile } */

void
f (_Bool *restrict x, _Bool *restrict y)
{
  for (int i = 0; i < 128; ++i)
    x[i] = x[i] == y[i];
}

/* { dg-final { scan-tree-dump "loop vectorized" "vect" { target vect_bool_cmp } } } */
