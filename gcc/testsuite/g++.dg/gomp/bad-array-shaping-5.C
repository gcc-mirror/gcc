// { dg-do compile }
// { dg-additional-options "-std=c++14" }

#include <string.h>
#include <assert.h>

template<typename T>
void foo (T *w)
{
  memset (w, 0, sizeof (T) * 100);
  int c = 50;

#pragma omp target enter data map(to: w[:100])

  for (int j = 0; j < 10; j++)
    for (int i = 0; i < 10; i++)
      w[j * 10 + i] = i + j * 3;

  /* No array-shaping inside a lambda body.  */
#pragma omp target update to([&](const int d) -> auto& { return ([d][d]) w; } (10))
// { dg-error {expected '\{' before '\[' token} "" { target *-*-* } .-1 }
// { dg-error {expected ';' before 'w'} "" { target *-*-* } .-2 }
// { dg-error {no match for 'operator\[\]' in} "" { target *-*-* } .-3 }

#pragma omp target exit data map(from: w[:100])
}

int main()
{
  float *arr = new float[100];
  int c = 50;

  memset (arr, 0, sizeof (float) * 100);

#pragma omp target enter data map(to: arr[:100])

  for (int j = 0; j < 10; j++)
    for (int i = 0; i < 10; i++)
      arr[j * 10 + i] = i + j * 3;

  /* As above.  */
#pragma omp target update to([&](const int d) -> auto& { return ([d][d]) arr; } (10))
// { dg-error {expected '\{' before '\[' token} "" { target *-*-* } .-1 }
// { dg-error {no match for 'operator\[\]' in} "" { target *-*-* } .-2 }
// { dg-error {expected ';' before 'arr'} "" { target *-*-* } .-3 }
// { dg-error {'#pragma omp target update' must contain at least one 'from' or 'to' clauses} "" { target *-*-* } .-4 }

#pragma omp target exit data map(from: arr[:100])

  foo<float> (arr);

  delete[] arr;

  return 0;
}
