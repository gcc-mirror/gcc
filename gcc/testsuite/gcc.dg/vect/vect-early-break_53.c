/* { dg-add-options vect_early_break } */
/* { dg-do compile } */
/* { dg-require-effective-target vect_early_break } */
/* { dg-require-effective-target vect_int } */

void abort ();
int a[64], b[64];
int main ()
{
  int c = 7;
  for (int i = 1; i < 64; ++i)
    if (b[i] != a[i] - a[i-1])
      abort ();
  if (b[0] != -7)
    abort ();
}

/* { dg-final { scan-tree-dump "vectorized 1 loops in function" "vect" } } */
