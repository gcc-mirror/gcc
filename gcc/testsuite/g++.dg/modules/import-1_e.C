// { dg-additional-options "-fmodules-ts -fdump-lang-module" }

import Foo;

int main ()
{
  Quux (1); // from Bar
  Quux (1, 2); // from Baz
  return 0;
}

// { dg-final { scan-lang-dump "Found exported import:1 Bar->1" "module" } }
// { dg-final { scan-lang-dump "Found exported import:2 Baz->2" "module" } }
