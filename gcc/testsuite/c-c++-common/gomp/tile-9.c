/* { dg-additional-options "-O2" } */

int foo (int);

void
mult (float *matrix1, float *matrix2, float *result,
      unsigned dim0, unsigned dim1, unsigned dim2, unsigned dim3)
{
  #pragma omp taskloop collapse(3)
  for (unsigned i = 0; i < dim0; i++)
    #pragma omp tile sizes(2, 2)
    #pragma omp tile sizes(2, 2)
    #pragma omp tile sizes(2, 2)
    for (unsigned j = 0; j < dim1; j += dim2 * foo (0))
      #pragma omp unroll partial(2)
      #pragma omp unroll partial(2)
      for (unsigned k = 0; k < dim1; k += dim3 * foo (1))
	result[i * dim1 + j] += matrix1[i * dim1 + k] * matrix2[k * dim0 + j];
}
