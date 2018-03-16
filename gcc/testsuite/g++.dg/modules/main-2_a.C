// { dg-module-do run { target *-*-* } "main-2-aux.s" }
// { dg-options "-fmodules-ts -fmodule-file=main-2-map" }
// Relies on CXX_MODULE_WRAPPER functionality, setting options above
// overrides the default -fmodule-wrapper=

import helgen;
// { dg-module-bmi "=wub.x" }
int main (void)
{
  greeter ("world");
  return 0;
}

