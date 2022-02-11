/* { dg-additional-options "-fdump-tree-original -save-temps" }  */
/* PR c++/51484  */

#define TEST(T) { \
  int fail = 0, trial; \
  for (int trial = 0; trial < TRIALS && fail == 0; trial++) { \
    _Pragma("omp target teams num_teams(1) thread_limit(1024)") \
     {T} \
  } \
}

#define TRIALS (1)
#define N (1024*3)

int main(void) {

  double C[N], D[N];
  double S[N];
  double p[2];
  int i; 
  for (i = 0; i < N; i++)
  {C[i] = 1; D[i] = i;}

  int max_threads = 224;

#define PARALLEL(X) TEST({ \
_Pragma("omp parallel if(threads[0] > 1) num_threads(threads[0])") \
{ \
_Pragma("omp for ordered") \
  X  \
_Pragma("omp for schedule(auto) ordered") \
  X  \
} \
})

  for (int t = 0; t <= max_threads; t += max_threads) {
    int threads[1]; threads[0] = t;
    S[0] = 0;
    PARALLEL(
    for (int i = 0; i < N; i++) { \
      _Pragma("omp ordered") \
      S[0] += C[i] + D[i]; \
    })
  }
  return 0;
}

/* On expansion, the _Pragma were wrongly placed, ensure the order is now correct: */
/* { dg-final { scan-tree-dump "#pragma omp target.*#pragma omp teams num_teams\\(1\\) thread_limit\\(1024\\).*#pragma omp parallel num_threads\\(threads\\\[0\\\]\\) if\\(threads\\\[0\\\] > 1\\).*#pragma omp for ordered.*#pragma omp ordered.*#pragma omp for ordered schedule\\(auto\\).*#pragma omp ordered" "original" } } */

