// { dg-do compile }

#include <string.h>
#include <assert.h>

template<typename T, int C, int D>
void foo (T *w)
{
  /* This isn't allowed. We get a cascade of errors because it looks a bit
     like lambda-definition syntax  */
#pragma omp target enter data map(to: ([C][D]) w[:100])
  // { dg-error {capture of non-variable 'C'} "" { target *-*-* } .-1 }
  // { dg-error {expected '\{' before '\[' token} "" { target *-*-* } .-2 }
  // { dg-warning {lambda expressions only available with} "" { target c++98_only } .-3 }
  // { dg-error {expected '\)' before 'w'} "" { target *-*-* } .-4 }
  // { dg-error {does not have pointer or array type} "" { target *-*-* } .-5 }

#pragma omp target exit data map(from: ([C][D]) w[:100])
  // { dg-error {capture of non-variable 'C'} "" { target *-*-* } .-1 }
  // { dg-error {expected '\{' before '\[' token} "" { target *-*-* } .-2 }
  // { dg-warning {lambda expressions only available with} "" { target c++98_only } .-3 }
  // { dg-error {expected '\)' before 'w'} "" { target *-*-* } .-4 }
  // { dg-error {does not have pointer or array type} "" { target *-*-* } .-5 }
}

int main()
{
  float *arr = new float[100];

  /* This isn't allowed (as above).  */
#pragma omp target enter data map(to: ([10][10]) arr[:100])
  // { dg-error {expected identifier before numeric constant} "" { target *-*-* } .-1 }
  // { dg-error {expected '\{' before '\[' token} "" { target *-*-* } .-2 }
  // { dg-warning {lambda expressions only available with} "" { target c++98_only } .-3 }
  // { dg-error {expected '\)' before 'arr'} "" { target *-*-* } .-4 }
  // { dg-error {no match for 'operator\[\]' in} "" { target *-*-* } .-5 }
  // { dg-error {'#pragma omp target enter data' must contain at least one 'map' clause} "" { target *-*-*} .-6 }

#pragma omp target exit data map(from: ([10][10]) arr[:100])
  // { dg-error {expected identifier before numeric constant} "" { target *-*-* } .-1 }
  // { dg-error {expected '\{' before '\[' token} "" { target *-*-* } .-2 }
  // { dg-warning {lambda expressions only available with} "" { target c++98_only } .-3 }
  // { dg-error {no match for 'operator\[\]' in} "" { target *-*-* } .-4 }
  // { dg-error {expected '\)' before 'arr'} "" { target *-*-* } .-5 }
  // { dg-error {'#pragma omp target exit data' must contain at least one 'map' clause} "" { target *-*-* } .-6 }

  foo<float, 5, 20> (arr);

  delete[] arr;

  return 0;
}
