// { dg-do compile }

int x;

template<typename T>
void foo()
{
  T arr[20];
  T *ptr;
  /* "arr[1:10]" looks like it might be an expression of array type, hence
     able to be indexed (again).  This isn't allowed, though.  */
#pragma omp target map(arr[1:10][2])
// { dg-error {'arr\[1\]' does not have pointer or array type} "" { target *-*-* } .-1 }
  { }
#pragma omp target map(arr[1:x][2])
// { dg-error {'arr\[1\]' does not have pointer or array type} "" { target *-*-* } .-1 }
  { }
  // ...and nor is this.
#pragma omp target map(ptr[1:10][2])
// { dg-error {'\*\(ptr \+ [0-9]+\)' does not have pointer or array type} "" { target *-*-* } .-1 }
  { }
#pragma omp target map(ptr[1:x][2])
// { dg-error {'\*\(ptr \+ [0-9]+\)' does not have pointer or array type} "" { target *-*-* } .-1 }
  { }
}

int main()
{
  int arr[20];
  int *ptr;
  /* "arr[1:10]" looks like it might be an expression of array type, hence
     able to be indexed (again).  This isn't allowed, though.  */
#pragma omp target map(arr[1:10][2])
// { dg-error {'arr\[1\]' does not have pointer or array type} "" { target *-*-* } .-1 }
  { }
#pragma omp target map(arr[1:x][2])
// { dg-error {'arr\[1\]' does not have pointer or array type} "" { target *-*-* } .-1 }
  { }
  // ...and nor is this.
#pragma omp target map(ptr[1:10][2])
// { dg-error {'\*\(ptr \+ [0-9]+\)' does not have pointer or array type} "" { target *-*-* } .-1 }
  { }
#pragma omp target map(ptr[1:x][2])
// { dg-error {'\*\(ptr \+ [0-9]+\)' does not have pointer or array type} "" { target *-*-* } .-1 }
  { }

  foo<int> ();

  return 0;
}
