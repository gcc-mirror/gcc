// { dg-additional-options "-fmodules-ts -fdump-lang-module-graph-blocks" }

export module malfoy;
// { dg-module-cmi malfoy }

import voldy;

void interpose ()
{
  // Force renumber of anon vars
  auto lambda = [] () {};
}

export auto conduit (int i)
{
  return frobber (i);
}

// { dg-final { scan-lang-dump-not {Cluster import } module } }
// { dg-final { scan-lang-dump-not {onnecting definition decl type_decl:'::frobber@voldy:.::._anon_1@voldy:.'} module } }
