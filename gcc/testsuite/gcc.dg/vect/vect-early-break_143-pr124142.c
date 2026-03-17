/* { dg-do compile } */
/* { dg-add-options vect_early_break } */
/* { dg-require-effective-target vect_early_break } */
/* { dg-require-effective-target vect_int } */

/* { dg-final { scan-tree-dump-not "LOOP VECTORIZED" "vect" } } */

void
__attribute__((noipa))
bug (int f, int *w, int l)
{
  int i;
  for (i = 0; i < l; ++i)
    while (f % w[i]) w[i]--;
}

