int a;

void
foo (int *c, int *d)
{
  #pragma omp for reduction (inscan, +: a) /* { dg-error "'a' specified in 'inscan' 'reduction' clause but not in 'scan' directive clause" } */
  #pragma omp tile sizes (2)
  for (int i = 0; i < 64; ++i)
    {
      a = a + c[i];
      #pragma omp scan inclusive (a) /* { dg-error "'#pragma omp scan' may only be used in a loop construct with 'inscan' 'reduction' clause" } */
      d[i] = a;
    }
}

void
bar (int **c, int **d)
{
  #pragma omp for collapse (2) reduction (inscan, +: a) /* { dg-error "'a' specified in 'inscan' 'reduction' clause but not in 'scan' directive clause" } */
  for (int i = 0; i < 64; ++i)
    #pragma omp tile sizes (3)
    for (int j = 0; j < 64; ++j)
      {
	d[i][j] = a;
	#pragma omp scan exclusive (a) /* { dg-error "'#pragma omp scan' may only be used in a loop construct with 'inscan' 'reduction' clause" } */
	a = a + c[i][j];
      }
}

void
baz (int *c, int *d)
{
  #pragma omp for reduction (inscan, +: a) /* { dg-error "'a' specified in 'inscan' 'reduction' clause but not in 'scan' directive clause" } */
  #pragma omp unroll partial (2)
  for (int i = 0; i < 64; ++i)
    {
      d[i] = a;
      #pragma omp scan exclusive (a) /* { dg-error "'#pragma omp scan' may only be used in a loop construct with 'inscan' 'reduction' clause" } */
      a = a + c[i];
    }
}

void
qux (int **c, int **d)
{
  #pragma omp for collapse (2) reduction (inscan, +: a) /* { dg-error "'a' specified in 'inscan' 'reduction' clause but not in 'scan' directive clause" } */
  for (int i = 0; i < 64; ++i)
    #pragma omp unroll partial (3)
    for (int j = 0; j < 64; ++j)
      {
	a = a + c[i][j];
	#pragma omp scan inclusive (a) /* { dg-error "'#pragma omp scan' may only be used in a loop construct with 'inscan' 'reduction' clause" } */
	d[i][j] = a;
      }
}
