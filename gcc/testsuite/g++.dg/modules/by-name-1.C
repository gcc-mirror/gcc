// check internals by name unless SCC
// { dg-additional-options "-fdump-lang-module" }

export module frob;
// { dg-module-bmi frob }

class X 
{
  int i;
};

export X *f ();

// { dg-final { scan-lang-dump {Wrote named decl:-[0-9]* type_decl:'::X'} "module" } }
// { dg-final { scan-lang-dump {Wrote named type:-[0-9]* record_type:'::X'} "module" } }
