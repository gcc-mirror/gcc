/* PR c/96678.  */

#define SIZE   (100)
typedef double Grid[SIZE];

void test (Grid src1)
{
  #pragma omp target map(alloc:src1[:])  /* { dg-error "for array function parameter length expression must be specified" }  */
  {
    src1[0] = 5;
  }
}

void test2 (double src2[])
{
  #pragma omp target map(alloc:src2[:])  /* { dg-error "for array function parameter length expression must be specified" }  */
  {
    src2[0] = 5;
  }
}

void test3 (double *src3)
{
  #pragma omp target map(alloc:src3[:])  /* { dg-error "for pointer type length expression must be specified" }  */
  {
    src3[0] = 5;
  }
}

