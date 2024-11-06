// { dg-additional-options "-fmodules-ts -fdump-lang-module-blocks-alias" }

export module foo;
// { dg-module-cmi foo }

export import :part2;
export import :part1;

export auto foo ()
{
  return frob::inner ();
}

// { dg-final { scan-lang-dump {Reading 2 pending entities keyed to '::frob'} module } }
// { dg-final { scan-lang-dump { Cluster members:\n  \[0\]=decl definition '::frob@foo:part1:1'\n  \[1\]=decl definition '::frob@foo:part1:1::inner@foo:part1:1'\n  \[2\]=decl declaration '::frob@foo:part1:1::inner@foo:part1:1::__dt '\n(  \[.\]=decl declaration '::frob@foo:part1:1::inner@foo:part1:1::__ct '\n)*  \[6\]=decl declaration '::frob@foo:part1:1::inner@foo:part1:1::inner@foo:part2:2'\n  \[7\]=decl declaration '::frob@foo:part1:1::frob@foo:part1:1'\n  \[8\]=decl declaration '::frob@foo:part1:1::__as_base @foo:part1:1'\n  \[9\]=binding '::frob'\n} module } }
// { dg-final { scan-lang-dump {Pendings 1} module } }
