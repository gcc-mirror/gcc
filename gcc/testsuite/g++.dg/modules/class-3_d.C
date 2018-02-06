// { dg-additional-options "-fdump-lang-module" }
// { dg-final { scan-lang-dump "Importing '::X'@One" module } }
// { dg-final { scan-lang-dump "Read:-\[0-9\]* record_type:'::X' imported type" module } }
// { dg-final { scan-lang-dump "Read interstitial type name type_decl:'::X'" module } }
// { dg-final { scan-lang-dump "Importing '::X::a'@One" module } }
// { dg-final { scan-lang-dump "Importing '::X::b'@One" module } }

import Two;

int main ()
{
  X x (0xdead, 0xbeef);

  if (x.a != 0xdead || x.b != 0xbeef)
    return 1;

  Frob (x);
  if (x.b != 0xdead)
    return 2;

  X y (0xcafe);
  if (y.a != 0xcafe || y.b != 0xcafe << 16)
    return 3;

  return 0;
}
