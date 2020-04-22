// { dg-additional-options "-fmodules-ts -fdump-lang-module-uid" }
export module Two;

export import One;

export inline void Frob (X &q)
{
  q.b = q.a;
}

// { dg-final { scan-lang-dump {Wrote import:-1 type_decl:'::X@One:.'} module } }
// { dg-final { scan-lang-dump {Indirect:-2 decl's type record_type:'::X@One:.'} module } }
// { dg-final { scan-lang-dump {Wrote import:-[0-9]* type_decl:'::X@One:.'@One} module } }
// { dg-final { scan-lang-dump {Indirect:-[0-9]* decl's type record_type:'::X@One:.'} module } }
// { dg-final { scan-lang-dump {Wrote import:-[0-9]* type_decl:'::X@One:.'@One} module } }
// { dg-final { scan-lang-dump {Indirect:-[0-9]* decl's type record_type:'::X@One:.'} module } }
// { dg-final { scan-lang-dump {Wrote member:-[0-9]* field_decl:'::X@One:.::a'} module } }
// { dg-final { scan-lang-dump {Wrote member:-[0-9]* field_decl:'::X@One:.::b'} module } }
