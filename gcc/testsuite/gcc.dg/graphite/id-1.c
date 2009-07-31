typedef int *lambda_vector;
typedef lambda_vector *lambda_matrix;
lambda_vector_add_mc (lambda_vector vec1, int const1,
		      lambda_vector vec2, int const2,
		      lambda_vector vec3, int size)
{
  int i;
  for (i = 0; i < size; i++)
    vec3[i] = const1 * vec1[i] + const2 * vec2[i];
}
lambda_matrix_add_mc (lambda_matrix mat1, int const1,
		      lambda_matrix mat2, int const2,
		      lambda_matrix mat3, int m, int n)
{
  int i;
  for (i = 0; i < m; i++)
    lambda_vector_add_mc (mat1[i], const1, mat2[i], const2, mat3[i], n);
}
