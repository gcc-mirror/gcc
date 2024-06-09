// { dg-do compile }

template<int C, int D>
void foo()
{
  int arr1[40];
#pragma omp target map(arr1[4,C:])
// { dg-warning "top-level comma expression in array subscript is deprecated" "" { target c++20_only } .-1 }
// { dg-error "cannot use multidimensional subscript in OpenMP array section" "" { target c++23 } .-2 }
  { }
#pragma omp target map(arr1[4,5:C,7])
// { dg-warning "top-level comma expression in array subscript is deprecated" "" { target c++20_only } .-1 }
// { dg-error "cannot use multidimensional subscript in OpenMP array section" "" { target c++23 } .-2 }
  { }
#pragma omp target map(arr1[:8,C,10])
// { dg-warning "top-level comma expression in array subscript is deprecated" "" { target c++20_only } .-1 }
// { dg-error "cannot use multidimensional subscript in OpenMP array section" "" { target c++23 } .-2 }
  { }
}

int main()
{
  int arr1[40];
#pragma omp target map(arr1[4,5:])
// { dg-warning "top-level comma expression in array subscript is deprecated" "" { target c++20_only } .-1 }
// { dg-error "cannot use multidimensional subscript in OpenMP array section" "" { target c++23 } .-2 }
  { }
#pragma omp target map(arr1[4,5:6,7])
// { dg-warning "top-level comma expression in array subscript is deprecated" "" { target c++20_only } .-1 }
// { dg-error "cannot use multidimensional subscript in OpenMP array section" "" { target c++23 } .-2 }
  { }
#pragma omp target map(arr1[:8,9,10])
// { dg-warning "top-level comma expression in array subscript is deprecated" "" { target c++20_only } .-1 }
// { dg-error "cannot use multidimensional subscript in OpenMP array section" "" { target c++23 } .-2 }
  { }

  foo<6, 9> ();

  return 0;
}

