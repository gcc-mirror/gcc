/* PR122128 */
/* { dg-do compile } */

_Bool a[1024];
signed char b[1024];

void foo ()
{
  for (int i = 0; i < 1024; ++i)
    {
      bool x = a[i];
      bool y = b[i] < 17;
      a[i] = x & y;
    }
}

/* { dg-final { scan-tree-dump "optimized: loop vectorized" "vect" { target vect_bool_cmp } } } */
