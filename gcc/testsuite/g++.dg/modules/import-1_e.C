// { dg-additional-options "-fmodules-ts -fdump-lang-module" }
// { dg-final { scan-lang-dump "Direct export import Baz" "module" } }
// { dg-final { scan-lang-dump "Direct export import Bar" "module" } }
import Foo;

int main ()
{
  Quux (1); // from Bar
  Quux (1, 2); // from Baz
  return 0;
}
