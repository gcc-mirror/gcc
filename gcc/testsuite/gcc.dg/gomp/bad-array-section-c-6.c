/* { dg-do compile } */

int x;

int main()
{
  int arr[20];
#pragma omp target map(arr[5:5] * 2)
/* { dg-error {invalid operands to binary \*} "" { target *-*-* } .-1 } */
  { }
#pragma omp target map(arr[x:5] * 2)
/* { dg-error {invalid operands to binary \*} "" { target *-*-* } .-1 } */
  { }

  return 0;
}
