/* PR c/100450 */
/* { dg-do compile } */
/* { dg-options "-fopenmp -save-temps -Wunknown-pragmas" } */

#define TEST(T) { \
     {T} \
}
#define CLAUSES reduction(+:red)
#define PARALLEL_FOR(X) TEST({ \
_Pragma("omp for CLAUSES") \
X \
})

void foo()
{
  int red = 0;
  int A[3] = {};
  #pragma omp parallel shared(red)
  PARALLEL_FOR( for(int i=0; i < 3; i++) red += A[i]; )
}
