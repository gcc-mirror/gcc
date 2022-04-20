/* { dg-do compile } */
/* { dg-require-effective-target arm_neon_ok } */
/* { dg-options "-mcpu=cortex-a15" } */
/* { dg-add-options arm_neon } */

typedef float stress_matrix_type_t;
typedef unsigned int size_t;
static void __attribute__((optimize("-O3"))) stress_matrix_xy_identity(
 const size_t n,
 stress_matrix_type_t a[restrict n][n],
 stress_matrix_type_t b[restrict n][n],
 stress_matrix_type_t r[restrict n][n])
{
 register size_t i;
 (void)a;
 (void)b;
 for (i = 0; i < n; i++) {
  register size_t j;
  for (j = 0; j < n; j++)
   r[i][j] = (i == j) ? 1.0 : 0.0;
   return;
 }
}
