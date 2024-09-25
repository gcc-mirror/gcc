/* { dg-do compile } */

int foo (void)
{
  volatile int arr[3] __attribute__((aligned(128))) = { 0 };
  return arr[2];
}
