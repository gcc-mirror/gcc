// { dg-module-do run { target *-*-* } "main-aux.s" }
// { dg-options "-fmodules-ts -fmodule-file=helgen=hell.x\\;main-aux.cc" }
// Relies on CXX_MODULE_WRAPPER functionality, setting options above
// overrides the default -fmodule-wrapper=

import helgen;
// { dg-module-bmi "=hell.x" }
int main (void)
{
  greeter ("world");
  return 0;
}

