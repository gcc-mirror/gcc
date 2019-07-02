// { dg-module-do run }
// { dg-additional-options "-fmodules-ts -fdump-lang-module-graph-blocks" }

export module voldy;
// { dg-module-cmi voldy }

export auto frobber (int i)
{
  return [=] (int j) { return i + j; };
}

// { dg-final { scan-lang-dump {Voldemort:0 '::frobber@voldy:1::._anon_0'} module } }
// { dg-final { scan-lang-dump {Connecting definition unnamed type_decl:'::frobber@voldy:1::._anon_0'} module } }
// { dg-final { scan-lang-dump {Unnamed 0 '::frobber@voldy:1::._anon_0' section:1} module } }
