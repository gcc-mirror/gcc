// { dg-additional-options "-fdump-lang-module" }
// { dg-final { scan-lang-dump "Writing interstitial type name type_decl:'::X'" module } }
// { dg-final { scan-lang-dump "Wrote import:-\[0-9\]* '::X'@One" module } }
// { dg-final { scan-lang-dump "Wrote imported type:-\[0-9\]* record_type:'::X'" module } }
// { dg-final { scan-lang-dump "Wrote import:-\[0-9\]* '::X::a'@One" module } }
// { dg-final { scan-lang-dump "Wrote import:-\[0-9\]* '::X::b'@One" module } }

export module Two;

export import One;

export inline void Frob (X &q)
{
  q.b = q.a;
}

