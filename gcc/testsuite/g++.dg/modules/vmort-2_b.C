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

// { dg-final { scan-lang-dump {Voldemort decl:0 \[0\] '::frobber@voldy:.::._anon_1@voldy:.'} module } }
// { dg-final { scan-lang-dump {Cluster import '::frobber@voldy:.::._anon_1@voldy:.'} module } }
// { dg-final { scan-lang-dump {Inserted:-1 horcrux:0@. for '::frobber@voldy:.::._anon_1@voldy:.'} module } }

