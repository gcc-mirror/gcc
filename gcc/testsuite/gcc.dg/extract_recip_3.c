/* { dg-do compile } */
/* { dg-options "-Ofast -fdump-tree-optimized-raw" } */

float
extract_square (float *a, float *b, float x, float y)
{
  *a = 3 / (y * y);
  *b = 5 / (y * y);

  return x / (y * y);
}

/* Don't expect the 'powmult' (calculation of y * y)
   to be deleted until a later pass, so look for one
   more multiplication than strictly necessary.  */
float
extract_recip (float *a, float *b, float x, float y, float z)
{
  *a = 7 / y;
  *b = x / (y * y);

  return z / y;
}

/* 4 multiplications in 'extract_square', and 4 in 'extract_recip'.  */
/* { dg-final { scan-tree-dump-times "mult_expr" 8 "optimized" } } */

/* 1 division in 'extract_square', 1 division in 'extract_recip'. */
/* { dg-final { scan-tree-dump-times "rdiv_expr" 2 "optimized" } } */
