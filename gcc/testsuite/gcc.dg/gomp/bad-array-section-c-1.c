/* { dg-do compile } */

int foo (int *ptr);

int main()
{
  int arr[20];
  /* Reject array section as function argument.  */
#pragma omp target map(foo(arr[3:5]))
/* { dg-error {expected '\]' before ':' token} "" { target *-*-* } .-1 } */
/* { dg-error {passing argument 1 of 'foo' makes pointer from integer without a cast} "" { target *-*-* } .-2 } */
/* { dg-message {sorry, unimplemented: unsupported map expression} "" { target *-*-* } .-3 } */
  { }

  return 0;
}
