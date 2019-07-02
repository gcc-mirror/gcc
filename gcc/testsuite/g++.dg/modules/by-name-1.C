// check internals by name unless SCC
// { dg-additional-options "-fmodules-ts -fdump-lang-module-uid" }

export module frob;
// { dg-module-cmi frob }

class X 
{
  int i;
};

export X *f ();

// { dg-final { scan-lang-dump {Wrote purview:-[0-9]* type_decl:'::X@frob:1'} "module" } }
// { dg-final { scan-lang-dump {Wrote decl's type:-[0-9]* record_type:'::X@frob:1'} "module" } }
