// { dg-do compile }

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

  /* This starts out looking like an array-shape cast.  Make sure it's still
     parsed as a lambda.  */
#pragma omp target update to(([c] (T *v) -> T { return v[c]; } (w)))
  // { dg-message {sorry, unimplemented: unsupported map expression} "" { target *-*-* } .-1 }
  // { dg-warning {lambda expressions only available with} "" { target c++98_only } .-2 }

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
#pragma omp target update to(([c] (float *v) -> float { return v[c]; } (arr)))
  // { dg-message {sorry, unimplemented: unsupported map expression} "" { target *-*-* } .-1 }
  // { dg-error {'#pragma omp target update' must contain at least one 'from' or 'to' clauses} "" { target *-*-* } .-2 }
  // { dg-warning {lambda expressions only available with} "" { target c++98_only } .-3 }

#pragma omp target exit data map(from: arr[:100])

  foo<float> (arr);

  delete[] arr;

  return 0;
}
