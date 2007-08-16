/* Check that execution counts and branch probabilities for various C
   constructs are reported correctly by gcov. */

#include <stdio.h>

/* { dg-options "-fprofile-arcs -ftest-coverage -fno-exceptions" } */
/* { dg-do run { target native } } */

class foo {
public:
  foo() { printf("foo()\n"); };
  ~foo() { printf("~foo()\n"); };
  void method(void) { printf("method()\n"); }; 
};

int func(int i, int j) __attribute__((noinline));

int func(int i, int j){
  if (j) {
    printf("unreachable\n");
    return 3;
  }

  foo f;

  if (i == 1) {
    f.method();
    f.method();
  } else {
    f.method();
    printf("unreachable\n");
    return 2;        /* count(#####) */
  }
  f.method();
  return 0;
}

int main() {
  return func(1, 0);
}
/* { dg-final { run-gcov gcov-4.C } } */
