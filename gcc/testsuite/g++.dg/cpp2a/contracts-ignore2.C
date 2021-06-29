// baseline for testing assert contracts being turned into compile time
// assumptions; see contracts-assume2 for the assumed case
// { dg-do run }
// { dg-options "-std=c++2a -fcontracts" }
#include <cstdio>

int fun(int x) {
  [[assert audit: x > 0]];
  if(x <= 0)
  {
    printf("%d: test x<=0 opt out\n", x);
    return -1;
  }
  else
  {
    printf("%d: test x>0\n", x);
    return 0;
  }
}

int main(int, char**) {
  volatile int x = -1;
  return fun(x);
}

// { dg-shouldfail "" }
