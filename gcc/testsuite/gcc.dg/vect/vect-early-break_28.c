/* { dg-add-options vect_early_break } */
/* { dg-do compile } */
/* { dg-require-effective-target vect_early_break } */
/* { dg-require-effective-target vect_int } */

void abort ();
int a[128];
int main ()
{
  int i;
  for (i = 1; i < 128; i++)
    if (a[i] != i%4 + 1)
    abort ();
}

/* { dg-final { scan-tree-dump "vectorized 1 loops in function" "vect" } } */
