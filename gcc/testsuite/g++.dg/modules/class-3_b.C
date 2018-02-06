// { dg-additional-options "-fdump-lang-module" }
// { dg-final { scan-lang-dump "Writing interstitial type name type_decl:'::X'" module } }
// { dg-final { scan-lang-dump "Writing imported '::X'@One" module } }
// { dg-final { scan-lang-dump "Writing:-\[0-9\]* record_type:'::X' imported type" module } }
// { dg-final { scan-lang-dump "Writing imported '::X::a'@One" module } }
// { dg-final { scan-lang-dump "Writing imported '::X::b'@One" module } }

export module Two;

export import One;

export inline void Frob (X &q)
{
  q.b = q.a;
}

