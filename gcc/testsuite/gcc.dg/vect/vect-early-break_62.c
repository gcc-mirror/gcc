/* { dg-add-options vect_early_break } */
/* { dg-do compile } */
/* { dg-require-effective-target vect_early_break } */
/* { dg-require-effective-target vect_int } */

int a, b;
int e() {
  int d, c;
  d = 0;
  for (; d < b; d++)
    a = 0;
  d = 0;
  for (; d < b; d++)
    if (d)
      c++;
  for (;;)
    if (c)
      break;
}

/* { dg-final { scan-tree-dump "vectorized 1 loops in function" "vect" } } */
