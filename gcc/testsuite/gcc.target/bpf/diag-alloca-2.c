/* { dg-do compile } */
/* { dg-options "-std=gnu89" } */

int
foo (int x)
{
  int arr[x];
  /* { dg-message {sorry, unimplemented: dynamic stack allocation not supported} {} { target *-*-* } .-1 } */

  return arr[3];
}
