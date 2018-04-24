// { dg-additional-options "-fdump-lang-module" }
// { dg-final { scan-lang-dump "Writing interstitial named type type_decl:'::X'" module } }
// { dg-final { scan-lang-dump "Wrote import:-\[0-9\]* type_decl:'::X'@One" module } }
// { dg-final { scan-lang-dump "Wrote named type:-\[0-9\]* record_type:'::X'" module } }
// { dg-final { scan-lang-dump "Wrote import:-\[0-9\]* field_decl:'::X::a'@One" module } }
// { dg-final { scan-lang-dump "Wrote import:-\[0-9\]* field_decl:'::X::b'@One" module } }

export module Two;

export import One;

export inline void Frob (X &q)
{
  q.b = q.a;
}

