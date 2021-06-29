// { dg-module-do run }
// { dg-additional-options "-fmodules-ts -fcontracts" }
module;
#include <cstdio>
export module baz;
import foo;
import bar;

int main(int, char**)
{
  int x = -1;

  printf("calling fn_int\n");
  fn_int(x--);
  printf("calling bar_fn_int\n");
  bar_fn_int(x--);

  return violation_count - 4;
}

// TODO verify dg-output as well once the testsuite supports it

