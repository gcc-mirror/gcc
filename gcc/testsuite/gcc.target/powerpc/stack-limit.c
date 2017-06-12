/* { dg-options "-O0 -fstack-limit-register=r14" } */

// PR80966

int foo (int i)
{
  char arr[135000];

  arr[i] = 0;
}
