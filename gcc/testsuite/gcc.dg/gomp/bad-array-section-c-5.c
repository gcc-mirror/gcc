/* { dg-do compile } */

int partly = 0;

int main()
{
  int arr[20];
#pragma omp target map(partly ? arr[5:5] : arr)
/* { dg-error {expected '\]' before ':' token} "" { target *-*-* } .-1 } */
/* { dg-error {pointer/integer type mismatch in conditional expression} "" { target *-*-* } .-2 } */
/* { dg-message {sorry, unimplemented: unsupported map expression} "" { target *-*-* } .-3 } */
  { }

  return 0;
}
