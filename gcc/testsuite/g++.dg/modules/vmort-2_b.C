// { dg-additional-options "-fmodules-ts -fdump-lang-module-graph-blocks" }

export module malfoy;
// { dg-module-bmi malfoy }

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

// { dg-final { scan-lang-dump {Voldemort decl:0 \[0\] '::frobber@voldy:2::._anon_1'} module } }
// { dg-final { scan-lang-dump {Cluster imported entity '::frobber@voldy:2::._anon_1'} module } }
// { dg-final { scan-lang-dump {Inserted:-1 horcrux:0@2 for '::frobber@voldy:2::._anon_1'} module } }

