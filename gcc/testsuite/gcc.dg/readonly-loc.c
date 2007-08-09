/* { dg-do compile } */
/* { dg-options "-O" } */
int func()
{
  const int *arr;
  const int arr2[5];
  arr[0] = 1; /* { dg-error "assignment of read-only location" "*(arr)" } */
  arr[1] = 1; /* { dg-error "assignment of read-only location" "*(arr + 4u)" } */
  arr2[0] = 1; /* { dg-error "assignment of read-only location" "arr2\[0\]" } */
  arr2[1] = 1; /* { dg-error "assignment of read-only location" "arr2\[1\]" } */
}
