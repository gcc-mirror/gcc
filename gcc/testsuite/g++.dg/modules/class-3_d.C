// { dg-additional-options "-fdump-lang-module" }
// { dg-final { scan-lang-dump "Imported:-\[0-9\]* '::X'@One" module } }
// { dg-final { scan-lang-dump "Read imported type:-\[0-9\]* record_type:'::X'" module } }
// { dg-final { scan-lang-dump "Read named type type_decl:'::X'" module } }
// { dg-final { scan-lang-dump "Imported:-\[0-9\]* '::X::a'@One" module } }
// { dg-final { scan-lang-dump "Imported:-\[0-9\]* '::X::b'@One" module } }

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
