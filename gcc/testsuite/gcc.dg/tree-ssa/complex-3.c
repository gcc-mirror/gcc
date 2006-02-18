/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */
int g(_Complex int*);
int f(void)
{
  _Complex int t = 0;
  int i, j;
 __real__ t += 2;
 __imag__ t += 2;
  return g(&t);
}

/* { dg-final { scan-tree-dump-times "__complex__" 0 "optimized" } } */
/* { dg-final { cleanup-tree-dump "optimized" } } */

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

