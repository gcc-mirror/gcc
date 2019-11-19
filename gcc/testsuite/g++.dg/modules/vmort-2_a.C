// { dg-module-do run }
// { dg-additional-options "-fmodules-ts -fdump-lang-module-graph-blocks" }

export module voldy;
// { dg-module-cmi voldy }

export auto frobber (int i)
{
  return [=] (int j) { return i + j; };
}

// { dg-final { scan-lang-dump {Connecting definition unnamed type_decl:'::frobber@voldy:.::._anon_0@voldy:.'} module } }
// { dg-final { scan-lang-dump {Entities 2} module } }

// { dg-final { scan-lang-dump {Cluster members:\n  \[0\]=decl declaration '::frobber@voldy:0'\n  \[1\]=unnamed definition '::frobber@voldy:0::._anon_0@voldy:0'\n  \[2\]=binding '::frobber'} module } }
