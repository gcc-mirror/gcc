/* { dg-do compile } */
/* { dg-options "-std=gnu89" } */

int
foo (int x)
{
  int arr[x]; /* { dg-error "support" } */
  return arr[3];
}
