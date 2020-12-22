// { dg-additional-options "-fmodules-ts -fdump-lang-module-blocks-alias" }

export module foo;
// { dg-module-cmi foo }

export import :part2;
export import :part1;


// { dg-final { scan-lang-dump { Cluster members:\n  \[0\]=decl definition '::frob@foo:part1:1'\n  \[1\]=decl declaration '::frob@foo:part1:1::frob@foo:part1:1'\n  \[2\]=decl definition '::frob@foo:part1:1::member@foo:part1:1'\n  \[3\]=decl declaration '::frob@foo:part1:1::__as_base @foo:part1:1'\n  \[4\]=binding '::frob'\n} module } }
// { dg-final { scan-lang-dump {Bindings 1} module } }
// { dg-final { scan-lang-dump {Pendings 0} module } }
// { dg-final { scan-lang-dump {Read:-[0-9]*'s named merge key .matched. function_decl:'::frob@foo:part1:1::member'} module } }

// { dg-final { scan-assembler-not {_ZN4frob6memberEv:} } }
