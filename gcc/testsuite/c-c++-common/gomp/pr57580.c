/* PR preprocessor/57580 */
/* { dg-do compile } */
/* { dg-options "-fopenmp -save-temps -fdump-tree-gimple" } */

#define PS \
  _Pragma("omp parallel num_threads(2)") \
  { \
    _Pragma("omp single") \
    { \
      ret = 0; \
    } \
  }

int
main ()
{
  int ret;
  _Pragma("omp parallel num_threads(3)")
  {
    _Pragma("omp single")
    {
      ret = 0;
    }
  }
  _Pragma("omp parallel num_threads(4)") { _Pragma("omp single") { ret = 0; } }
  { _Pragma("omp parallel num_threads(5)") { _Pragma("omp single") { ret = 0; } } }
  PS
  PS
  return ret;
}

/* { dg-final { scan-tree-dump-times "#pragma omp parallel\[^\n\r]*num_threads\\(2\\)" 2 "gimple" } } */
/* { dg-final { scan-tree-dump-times "#pragma omp parallel\[^\n\r]*num_threads\\(3\\)" 1 "gimple" } } */
/* { dg-final { scan-tree-dump-times "#pragma omp parallel\[^\n\r]*num_threads\\(4\\)" 1 "gimple" } } */
/* { dg-final { scan-tree-dump-times "#pragma omp parallel\[^\n\r]*num_threads\\(5\\)" 1 "gimple" } } */
/* { dg-final { scan-tree-dump-times "#pragma omp single" 5 "gimple" } } */
