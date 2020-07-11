// { dg-do run { target c++20 } }
// { dg-additional-sources "lambda-uneval9.cc" }

#include "lambda-uneval9.h"
int foo() { return f(); }
extern int bar();

int main()
{
  if (foo() != 1) __builtin_abort();
  if (bar() != 2) __builtin_abort();
}
