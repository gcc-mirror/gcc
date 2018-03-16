// { dg-module-do run { target *-*-* } "main-3-aux.s" }
// { dg-options "-fmodules-ts -fmodule-file=main-3-map" }
// Relies on CXX_MODULE_WRAPPER functionality, setting options above
// overrides the default -fmodule-wrapper=

import helgen;
// { dg-module-bmi "=wno.x" }
int main (void)
{
  greeter ("world");
  return 0;
}

