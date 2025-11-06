/* { dg-do compile } */
/* { dg-require-effective-target vect_int } */
/* { dg-require-effective-target vect_unpack } */

int count_true(const bool *values, int len)
{
  int count = 0;
  for (int i = 0; i < len; i++)
    count += values[i];
  return count;
}

/* { dg-final { scan-tree-dump "optimized: loop vectorized" "vect" } } */
