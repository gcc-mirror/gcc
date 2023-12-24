/* { dg-add-options vect_early_break } */
/* { dg-do compile } */
/* { dg-require-effective-target vect_early_break } */
/* { dg-require-effective-target vect_int } */

int in[100];
int out[100 * 2];

int main (void)
{
  if (out[0] != in[100 - 1])
  for (int i = 1; i <= 100; ++i)
    if (out[i] != 2)
      __builtin_abort ();
}

/* { dg-final { scan-tree-dump "vectorized 1 loops in function" "vect" } } */
