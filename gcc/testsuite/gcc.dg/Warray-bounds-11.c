/* { dg-do compile } */
/* { dg-options "-O2 -Warray-bounds -std=gnu99" } */
/* Test zero-length arrays for GNU C.  */

unsigned int a[] = { };
unsigned int size_a;

int test(void)
{
  /* This should not warn.  */
  return size_a ? a[0] : 0;
}
