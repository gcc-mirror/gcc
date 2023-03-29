// ensure that assert contracts can be turned into compile time assumptions
// and that they can be used for optimization.
//
// Even though x == -1, the assert contract tells the compiler that it is
// safe to assume the x <= 0 branch is never taken fun can be transformed into
// just
//   printf("%d: test x>0\n", x);
//   return 0;
// we ensure this by matching on the output and expecting a 0 return code from
// main -- unlike contracts-ignore2 which expects a failing return code
// { dg-do run }
// { dg-options "-std=c++2a -fcontracts -fcontract-role=default:never,assume,ignore -O1" }
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

// { dg-output "-1: test x>0(\n|\r\n|\r)" }
