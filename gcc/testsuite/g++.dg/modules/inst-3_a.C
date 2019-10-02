// { dg-module-do run }
// { dg-additional-options {-fmodules-ts -fdump-lang-module-graph-blocks-alias} }

export module foo;
// { dg-module-cmi foo }

export template<typename T> struct TPL
{
  T m;
};

export inline int user (int i)
{
  TPL<int> x;
  x.m = i;
  return x.m;
}

// { dg-final { scan-lang-dump {Cluster members:\n  \[0\]=specialization definition '::TPL@foo:.<int>'\n  \[[0-9]*\]=specialization declaration '::TPL@foo:.<int>::TPL@foo:.<int>'(\n  \[[0-9]*\]=clone (declaration|definition) '::foo[^\n]* ')*} module } }
// { dg-final { scan-lang-dump {Writing type spec key for mergeable specialization type_decl:'::TPL@foo:.<int>'} module } }
// { dg-final { scan-lang-dump {Depset:0 specialization type_decl:'::TPL@foo:.<int>'} module } }
// { dg-final { scan-lang-dump {Voldemort:0 '::TPL@foo:.<int>'} module } }
// { dg-final { scan-lang-dump {Inserted:-1 horcrux:0@0 for '::TPL@foo:.<int>'} module } }
