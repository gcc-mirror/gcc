/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-optimized" } */

typedef _Complex float COMPLEX_FLOAT;
float real_part(COMPLEX_FLOAT a)
{
  return *(float*)(&a);
}

float real_part_2(COMPLEX_FLOAT a)
{
  return ((float*)(&a))[0];
}


float imag_part(COMPLEX_FLOAT a)
{
  return ((float*)(&a))[1];
}

/* Test that the above gets optimized to REALPART_EXPR and IMAGPART_EXPR
   respectively. */

/* { dg-final { scan-tree-dump-times "REALPART_EXPR" 2 "optimized" } } */
/* { dg-final { scan-tree-dump-times "IMAGPART_EXPR" 1 "optimized" } } */
/* { dg-final { cleanup-tree-dump "optimized" } } */

