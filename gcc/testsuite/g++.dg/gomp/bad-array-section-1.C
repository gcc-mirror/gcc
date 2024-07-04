// { dg-do compile }

int foo (int *ptr);

template<typename T>
T baz (T *ptr);

template<typename T>
void bar()
{
  T arr[20];

#pragma omp target map(baz(arr[3:5]))
// { dg-error {expected '\]' before ':' token} "" { target *-*-* } .-1 }
// { dg-error {expected '\)' before ':' token} "" { target *-*-* } .-2 }
// { dg-error {expected '\)' before '\]' token} "" { target *-*-* } .-3 }
// { dg-error {expected an OpenMP clause before '\]' token} "" { target *-*-* } .-4 }
  { }
}

int main()
{
  int arr[20];
  // Reject array section as function argument.
#pragma omp target map(foo(arr[3:5]))
// { dg-error {expected '\]' before ':' token} "" { target *-*-* } .-1 }
// { dg-error {expected '\)' before ':' token} "" { target *-*-* } .-2 }
// { dg-error {expected '\)' before '\]' token} "" { target *-*-* } .-3 }
// { dg-error {expected an OpenMP clause before '\]' token} "" { target *-*-* } .-4 }
  { }

  bar<int> ();

  return 0;
}
