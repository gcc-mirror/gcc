// { dg-additional-options "-fmodules-ts -fdump-lang-module-uid" }
export module Two;

export import One;

export inline void Frob (X &q)
{
  q.b = q.a;
}

// { dg-final { scan-lang-dump {Writing typedef type_decl:'::X@One:2'} module } }
// { dg-final { scan-lang-dump {Wrote import:-[0-9]* type_decl:'::X@One:2'@One} module } }
// { dg-final { scan-lang-dump {Wrote decl's type:-[0-9]* record_type:'::X@One:2'} module } }
// { dg-final { scan-lang-dump {Wrote import:-[0-9]* type_decl:'::X@One:2'@One} module } }
// { dg-final { scan-lang-dump {Wrote decl's type:-[0-9]* record_type:'::X@One:2'} module } }
// { dg-final { scan-lang-dump {Wrote import:-[0-9]* field_decl:'::X@One:2::a'@One} module } }
// { dg-final { scan-lang-dump {Wrote import:-[0-9]* field_decl:'::X@One:2::b'@One} module } }
