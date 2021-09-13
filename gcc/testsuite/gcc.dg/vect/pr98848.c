/* PR tree-optimization/98848 */
/* { dg-do compile } */
/* { dg-require-effective-target vect_int } */

short a[9000];

int
foo (void)
{ 
  int b = a[0];
  int i;
  for (i = 1; i < 9000; i ++)
    if (a[i] < b)
      b = a[i];
  return b;
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loop" 1 "vect" { xfail { vect_no_int_add || vect_no_int_min_max } } } } */
