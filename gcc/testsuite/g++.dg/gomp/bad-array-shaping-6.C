// { dg-do compile }

#include <string.h>
#include <assert.h>

template<typename T>
void foo (T *w)
{
  memset (w, 0, sizeof (T) * 100);

#pragma omp target enter data map(to: w[:100])

  for (int j = 0; j < 10; j++)
    for (int i = 0; i < 10; i++)
      w[j * 10 + i] = i + j * 3;

  /* No array-shaping inside a statement expression.  */
#pragma omp target update to( ({ int d = 10; ([d][d]) w; )} )
// { dg-error {expected '\{' before '\[' token} "" { target *-*-* } .-1 }
// { dg-warning {lambda expressions only available with} "" { target c++98_only } .-2 }
// { dg-error {no match for 'operator\[\]'} "" { target *-*-* } .-3 }
// { dg-error {expected ';' before 'w'} "" { target *-*-* } .-4 }
// { dg-error {expected primary-expression before '\)' token} "" { target *-*-* } .-5 }
// { dg-error {expected '\)' before end of line} "" { target *-*-* } .-6 }
// { dg-message {sorry, unimplemented: unsupported map expression} "" { target *-*-* } .-7 }

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

  /* As above.  */
#pragma omp target update to( ({ int d = 10; ([d][d]) arr; )} )
// { dg-error {expected '\{' before '\[' token} "" { target *-*-* } .-1 }
// { dg-warning {lambda expressions only available with} "" { target c++98_only } .-2 }
// { dg-error {no match for 'operator\[\]'} "" { target *-*-* } .-3 }
// { dg-error {expected primary-expression before '\)' token} "" { target *-*-* } .-4 }
// { dg-error {expected '\)' before end of line} "" { target *-*-* } .-5 }
// { dg-message {sorry, unimplemented: unsupported map expression} "" { target *-*-* } .-6 }
// { dg-error {'#pragma omp target update' must contain at least one 'from' or 'to' clauses} "" { target *-*-* } .-7 }

#pragma omp target exit data map(from: arr[:100])

  foo<float> (arr);

  delete[] arr;

  return 0;
}
