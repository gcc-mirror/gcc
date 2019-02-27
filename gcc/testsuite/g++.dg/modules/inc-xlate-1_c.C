// { dg-additional-options "-fmodules-ts -fmodule-mapper=|cxx-mapper\\ -f\\ $srcdir/g++.dg/modules/inc-xlate-1.map" }

extern "C" {
  __import <stdarg.h>;
}

int main ()
{
  frob ();
  return 0;
}
