// { dg-module-do run }
// { dg-additional-options "-fmodules-ts -fcontracts" }
// { dg-skip-if "requires hosted libstdc++ for cstdio" { ! hostedlib } }

#include <cstdio>
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

