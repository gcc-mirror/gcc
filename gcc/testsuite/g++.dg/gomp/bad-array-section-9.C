// { dg-do compile }

int x;

template<typename T>
void foo()
{
  T arr1[40];
  T arr2[40];
#pragma omp target map(arr1[arr2[4:5]:arr2[6:7]])
// { dg-error {low bound 'arr2\[4:5\]' of array section does not have integral type} "" { target *-*-* } .-1 }
  { }
#pragma omp target map(arr1[arr2[:1]:arr2[6:1]])
// { dg-error {low bound 'arr2\[:1\]' of array section does not have integral type} "" { target *-*-* } .-1 }
  { }
#pragma omp target map(arr1[x:arr2[6:1]])
// { dg-error {length 'arr2\[6:1\]' of array section does not have integral type} "" { target *-*-* } .-1 }
  { }
}

int main()
{
  int arr1[40];
  int arr2[40];
#pragma omp target map(arr1[arr2[4:5]:arr2[6:7]])
// { dg-error {low bound 'arr2\[4:5\]' of array section does not have integral type} "" { target *-*-* } .-1 }
  { }
#pragma omp target map(arr1[arr2[:1]:arr2[6:1]])
// { dg-error {low bound 'arr2\[:1\]' of array section does not have integral type} "" { target *-*-* } .-1 }
  { }
#pragma omp target map(arr1[x:arr2[6:1]])
// { dg-error {length 'arr2\[6:1\]' of array section does not have integral type} "" { target *-*-* } .-1 }
  { }

  foo<int> ();

  return 0;
}

