// check internals by name unless SCC
// { dg-additional-options "-fmodules-ts -fdump-lang-module-uid" }

export module frob;
// { dg-module-cmi frob }

class X 
{
  int i;
};

export X *f ();

// { dg-final { scan-lang-dump {Wrote purview:-[0-9]* type_decl:'::X'} "module" } }
// { dg-final { scan-lang-dump {Indirect:-[0-9]* decl's type record_type:'::X'} "module" } }
