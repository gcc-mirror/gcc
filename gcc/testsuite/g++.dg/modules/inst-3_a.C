// { dg-module-do run }
// { dg-additional-options {-fmodules-ts -fdump-lang-module-graph-blocks-alias} }

export module foo;
// { dg-module-bmi foo }

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

// { dg-final { scan-lang-dump {Cluster:2 2 depsets\n  \[0\]=specialization definition '::TPL@foo:1<int>'\n  \[1\]=specialization declaration '::TPL@foo:1<int>::TPL<int>'} module } }
// { dg-final { scan-lang-dump {Wrote:-[0-9]* global specialization type_decl:'::TPL@foo:1<int>'} module } }
// { dg-final { scan-lang-dump {Depset:0 specialization type_decl:'::TPL@foo:1<int>'} module } }
// { dg-final { scan-lang-dump {Voldemort:0 '::TPL@foo:1<int>'} module } }
// { dg-final { scan-lang-dump {Inserted:-1 horcrux:0@0 for '::TPL@foo:1<int>'} module } }
