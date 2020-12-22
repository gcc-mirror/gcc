// { dg-module-do run }
// { dg-additional-options "-fmodules-ts -fdump-lang-module-graph-blocks" }

export module voldy;
// { dg-module-cmi voldy }

export auto frobber (int i)
{
  return [=] (int j) { return i + j; };
}

// { dg-final { scan-lang-dump {Connecting definition decl type_decl:'::frobber::._anon_0'} module } }
// { dg-final { scan-lang-dump {Entities 5} module } }

// { dg-final { scan-lang-dump {Cluster members:\n  \[0\]=decl declaration '::frobber'\n  \[1\]=decl definition '::frobber::._anon_0'\n(  \[.\]=decl [^\n]*'\n)*  \[.\]=binding '::frobber'} module } }
