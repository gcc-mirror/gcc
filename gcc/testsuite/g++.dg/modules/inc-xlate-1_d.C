// { dg-additional-options "-fmodules-ts -fmodule-mapper=|cxx-mapper\\ -f\\ $srcdir/g++.dg/modules/inc-xlate-1.map" }

typedef int import;

extern "C" {
  import var; // ok
}

__import name; // { dg-error "header unit name" }

int main ()
{
  return 0;
}
