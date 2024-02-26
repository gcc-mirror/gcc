/* { dg-do compile } */
/* { dg-add-options vect_early_break } */
/* { dg-require-effective-target vect_early_break } */
/* { dg-require-effective-target vect_int } */
/* { dg-additional-options "-O3" } */

/* { dg-final { scan-tree-dump "LOOP VECTORIZED" "vect" } } */

int m;
void __attribute__((noreturn)) n();
void t1(int jj, int l) {
  for (int i = 1; i < l; i++)
  {
    int p = m++;
    if (p)
      n();
    if(jj <= i)
      __builtin_unreachable();
  }
}
