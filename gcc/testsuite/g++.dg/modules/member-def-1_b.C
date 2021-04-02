// { dg-additional-options "-fmodules-ts -fdump-lang-module-blocks" }

export module foo:part2;
// { dg-module-cmi {foo:part2} }

import :part1;

struct frob::inner
{
  int i;
};

// { dg-final { scan-lang-dump { Cluster members:\n  \[0\]=decl definition '::frob@foo:part1:1::inner'\n  \[1\]=decl declaration '::frob@foo:part1:1::inner::inner'\n} module } }
// { dg-final { scan-lang-dump {Pending member '::frob@foo:part1:1::inner' entity:0 section:. keyed to '::frob'} module } }
