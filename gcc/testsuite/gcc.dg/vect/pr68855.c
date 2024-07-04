/* { dg-do compile } */
/* { dg-require-effective-target vect_float } */

/* { dg-final { scan-tree-dump "LOOP VECTORIZED" "vect" } } */

/* PAREN_EXPR should not cause the vectorization of complex float add to be missed. */
void foo(_Complex float *a, int n)
{
  for(int i = 0; i < n; i++)
  {
    _Complex float t;
    t = a[i];
    t += 6.0;
    t = __builtin_assoc_barrier(t);
    a[i] = t;
  }
}
