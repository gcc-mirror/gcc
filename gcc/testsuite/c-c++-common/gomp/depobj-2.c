/* PR c++/98072 */

typedef struct __attribute__((__aligned__ (sizeof (void *)))) omp_depend_t {
  char __omp_depend_t__[2 * sizeof (void *)];
} omp_depend_t;

void
foo (int *x, omp_depend_t *y, int z)
{
  #pragma omp depobj (*y) depend (in: x[z])
}
