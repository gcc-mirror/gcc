// { dg-additional-options "-fmodules-ts -fdump-lang-module-uid" }
export module Two;

export import One;

export inline void Frob (X &q)
{
  q.b = q.a;
}

// { dg-final { scan-lang-dump {Writing interstitial named type type_decl:'::X@2\(One\)'} module } }
// { dg-final { scan-lang-dump {Wrote import:-[0-9]* type_decl:'::X@2\(One\)'@One} module } }
// { dg-final { scan-lang-dump {Wrote decl's type:-[0-9]* record_type:'::X@2\(One\)'} module } }
// { dg-final { scan-lang-dump {Wrote import:-[0-9]* field_decl:'::X@2\(One\)::a'@One} module } }
// { dg-final { scan-lang-dump {Wrote import:-[0-9]* field_decl:'::X@2\(One\)::b'@One} module } }
