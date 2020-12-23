// { dg-additional-options "-fmodules-ts -fdump-lang-module-blocks" }

export module foo:part2;
// { dg-module-cmi {foo:part2} }

import :part1;

inline void frob::member ()
{
}

// { dg-final { scan-lang-dump { Cluster members:\n  \[0\]=decl definition '::frob@foo:part1:1::member'\n} module } }
// { dg-final { scan-lang-dump {Pendings 1} module } }
// { dg-final { scan-lang-dump {Bindings 0} module } }

// { dg-final { scan-assembler-not {_ZN4frob6memberEv:} } }
