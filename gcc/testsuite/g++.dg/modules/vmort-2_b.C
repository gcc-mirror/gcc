// { dg-additional-options "-fmodules-ts -fdump-lang-module-graph-blocks" }

export module malfoy;
// { dg-module-bmi malfoy }

import voldy;

export auto conduit (int i)
{
  return frobber (i);
}

// { dg-final { scan-lang-dump {Voldemort decl:0 \[0\] '::frobber::._0'} module } }
// { dg-final { scan-lang-dump {Cluster imported entity '::frobber::._0'} module } }
// { dg-final { scan-lang-dump {Inserted:-1 horcrux:0@2 for '::frobber::._0'} module } }

