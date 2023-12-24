/* { dg-add-options vect_early_break } */
/* { dg-do compile } */
/* { dg-require-effective-target vect_early_break } */
/* { dg-require-effective-target vect_int } */

int main (void)
{
  signed char a[50], b[50], c[50];
  for (int i = 0; i < 50; ++i)
    if (a[i] != ((((signed int) -1 < 0 ? -126 : 4) + ((signed int) -1 < 0 ? -101 : 26) + i * 9 + 0) >> 1))
      __builtin_abort ();
}

/* { dg-final { scan-tree-dump "vectorized 1 loops in function" "vect" } } */
