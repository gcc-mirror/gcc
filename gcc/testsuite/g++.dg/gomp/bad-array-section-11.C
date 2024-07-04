// { dg-do compile }
// { dg-additional-options "-std=c++23" }

template<int C, int D>
void foo()
{
  int arr1[40];
#pragma omp target map(arr1[4,C:])
// { dg-error "cannot use multidimensional subscript in OpenMP array section" "" { target *-*-* } .-1 }
  { }
#pragma omp target map(arr1[4,5:C,7])
// { dg-error "cannot use multidimensional subscript in OpenMP array section" "" { target *-*-* } .-1 }
  { }
#pragma omp target map(arr1[:8,C,10])
// { dg-error "cannot use multidimensional subscript in OpenMP array section" "" { target *-*-* } .-1 }
  { }
}

int main()
{
  int arr1[40];
#pragma omp target map(arr1[4,5:])
// { dg-error "cannot use multidimensional subscript in OpenMP array section" "" { target *-*-* } .-1 }
  { }
#pragma omp target map(arr1[4,5:6,7])
// { dg-error "cannot use multidimensional subscript in OpenMP array section" "" { target *-*-* } .-1 }
  { }
#pragma omp target map(arr1[:8,9,10])
// { dg-error "cannot use multidimensional subscript in OpenMP array section" "" { target *-*-* } .-1 }
  { }

  foo<6, 9> ();

  return 0;
}

