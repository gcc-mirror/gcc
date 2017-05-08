// { dg-additional-options "-fdump-front-end" }
// { dg-final { scan-dump "fe" "Direct export import 'Baz'" "fe" } }
// { dg-final { scan-dump "fe" "Direct export import 'Bar'" "fe" } }
import Foo;

int main ()
{
  Quux (1); // from Bar
  Quux (1, 2); // from Baz
  return 0;
}
