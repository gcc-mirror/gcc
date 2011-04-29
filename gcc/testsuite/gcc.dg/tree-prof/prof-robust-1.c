/* { dg-options "-O2 -w" } */

#include <stdio.h>
#include <stdlib.h>

#ifdef _PROFILE_USE
int foo(int x) {
  return 3 * x;
}
#else
int foo(int x) {
  return 3 * x;
}
#endif

int x = 1000;

int main(int argc, char *argv[]) {
  int i;
  int sum = 0;
  for (i = 0; i < x; i++)
    sum += i;
  printf("%d\n", sum);
  return 0;
}
