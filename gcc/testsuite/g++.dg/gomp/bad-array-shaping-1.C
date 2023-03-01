// { dg-do compile }

#include <string.h>
#include <assert.h>

template<typename T, int C, int D>
void foo (T *w)
{
  memset (w, 0, sizeof (T) * 100);

#pragma omp target enter data map(to: w[:100])

  for (int j = 0; j < 10; j++)
    for (int i = 0; i < 10; i++)
      w[j * 10 + i] = i + j * 3;

#pragma omp target update to(([C][D]) w[3:2][1:8][0:5])
// { dg-error "too many array section specifiers for" "" { target *-*-* } .-1 }
// { dg-error "'#pragma omp target update' must contain at least one 'from' or 'to' clauses" "" { target *-*-* } .-2 }

#pragma omp target exit data map(from: w[:100])
}

int main()
{
  float *arr = new float[100];

  memset (arr, 0, sizeof (float) * 100);

#pragma omp target enter data map(to: arr[:100])

  for (int j = 0; j < 10; j++)
    for (int i = 0; i < 10; i++)
      arr[j * 10 + i] = i + j * 3;

#pragma omp target update to(([10][10]) arr[3:2][1:8][0:5])
// { dg-error "too many array section specifiers for" "" { target *-*-* } .-1 }
// { dg-error "'#pragma omp target update' must contain at least one 'from' or 'to' clauses" "" { target *-*-* } .-2 }

#pragma omp target exit data map(from: arr[:100])

  foo<float, 5, 20> (arr);

  delete[] arr;

  return 0;
}
