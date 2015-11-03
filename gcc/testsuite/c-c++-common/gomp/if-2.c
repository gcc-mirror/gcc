/* { dg-do compile } */
/* { dg-options "-fopenmp" } */

void
foo (int a, int b, int *p, int *q, int task)
{
  int i;
  #pragma omp parallel if (a) if (b) /* { dg-error "too many .if. clauses without modifier" } */
    ;
  #pragma omp parallel if (a) if (parallel: b) /* { dg-error "if any .if. clause has modifier, then all .if. clauses have to use modifier" } */
    ;
  #pragma omp parallel if (parallel: a) if (b) /* { dg-error "if any .if. clause has modifier, then all .if. clauses have to use modifier" } */
    ;
  #pragma omp parallel if (parallel:a) if (parallel:a) /* { dg-error "too many .if. clauses with .parallel. modifier" } */
    ;
  #pragma omp parallel if (task:a) /* { dg-error "expected .parallel. .if. clause modifier rather than .task." } */ \
    if (taskloop: b) /* { dg-error "expected .parallel. .if. clause modifier rather than .taskloop." } */
    ;
  #pragma omp parallel if (target update:a) /* { dg-error "expected .parallel. .if. clause modifier rather than .target update." } */
    ;
  #pragma omp parallel for simd if (target update: a) /* { dg-error "expected .parallel. .if. clause modifier rather than .target update." } */
  for (i = 0; i < 16; i++)
    ;
  #pragma omp task if (task)
    ;
  #pragma omp task if (task: task)
    ;
  #pragma omp task if (parallel: a) /* { dg-error "expected .task. .if. clause modifier rather than .parallel." } */
    ;
  #pragma omp taskloop if (task : a) /* { dg-error "expected .taskloop. .if. clause modifier rather than .task." } */
  for (i = 0; i < 16; i++)
    ;
  #pragma omp target if (taskloop: a) /* { dg-error "expected .target. .if. clause modifier rather than .taskloop." } */
    ;
  #pragma omp target teams distribute parallel for simd if (target exit data : a) /* { dg-error "expected .parallel. or .target. .if. clause modifier" } */
  for (i = 0; i < 16; i++)
    ;
  #pragma omp target data if (target: a) map (p[0:2]) /* { dg-error "expected .target data. .if. clause modifier rather than .target." } */
    ;
  #pragma omp target enter data if (target data: a) map (to: p[0:2]) /* { dg-error "expected .target enter data. .if. clause modifier rather than .target data." } */
  #pragma omp target exit data if (target enter data: a) map (from: p[0:2]) /* { dg-error "expected .target exit data. .if. clause modifier rather than .target enter data." } */
  #pragma omp target update if (target exit data:a) to (q[0:3]) /* { dg-error "expected .target update. .if. clause modifier rather than .target exit data." } */
}
