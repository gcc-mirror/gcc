// { dg-additional-options "-fmodules-ts -fdump-lang-module" }
// { dg-final { scan-lang-dump "Read exported import:2 Bar->2" "module" } }
// { dg-final { scan-lang-dump "Read exported import:3 Baz->3" "module" } }
import Foo;

int main ()
{
  Quux (1); // from Bar
  Quux (1, 2); // from Baz
  return 0;
}
