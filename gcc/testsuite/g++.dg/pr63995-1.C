/* { dg-do compile { target { { i?86-*-* x86_64-*-* } && { ! x32 } } } } */
/* { dg-options "-O2 -g -fcheck-pointer-bounds -mmpx" } */

int test1 (int i)
{
  extern const int arr[10];
  return arr[i];
}

extern const int arr[10];

int test2 (int i)
{
  return arr[i];
}
