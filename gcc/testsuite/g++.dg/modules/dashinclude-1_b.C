// Test that include translation works with command-line -include.
// { dg-additional-options "-fmodules-ts -fdump-lang-module -include $srcdir/g++.dg/modules/dashinclude-1_a.H" }

int main ()
{
  return f();
}

// { dg-final { scan-lang-dump {Translating include to import} module } }
