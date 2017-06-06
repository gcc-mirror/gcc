// { dg-additional-options "-fdump-lang-module" }
// { dg-final { scan-lang-dump "Importing ::::'X'@One" module } }
// { dg-final { scan-lang-dump "Read:\[0-9\]* record_type:'X' imported type" module } }
// { dg-final { scan-lang-dump "Read interstitial type name type_decl:'X'" module } }
// { dg-final { scan-lang-dump "Importing X::'a'@One" module } }
// { dg-final { scan-lang-dump "Importing X::'b'@One" module } }

import Two;

int main ()
{
  X x;

  x.a = 0xfeed;
  x.b = 0xface;
  Frob (x);
  return x.b != 0xfeed;
}
