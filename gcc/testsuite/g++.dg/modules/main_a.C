// { dg-module-do run { target *-*-* } "hello.o" }
// { dg-options "-fmodules -fmodule-file=main-map" }
// Relies on CXX_MODULE_WRAPPER functionality, setting options above
// overrides the default -fmodule-wrapper=

import helgen;
// { dg-module-bmi "=hell.x" }
int main (void)
{
  greeter ("world");
  return 0;
}

