// { dg-additional-options "-fdump-lang" }
// { dg-final { scan-dump "lang" "Direct export import 'Baz'" "fe" } }
// { dg-final { scan-dump "lang" "Direct export import 'Bar'" "fe" } }
import Foo;

int main ()
{
  Quux (1); // from Bar
  Quux (1, 2); // from Baz
  return 0;
}
