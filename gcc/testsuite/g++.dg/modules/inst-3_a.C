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

// { dg-final { scan-lang-dump {Cluster members:\n  \[0\]=specialization definition '::TPL@foo:1<int>'\n(  \[.\]=clone (declaration|definition) '::hidey[^\n]* '\n)*  \[.*\]=specialization declaration '::TPL@foo:1<int>::TPL<int>'} module } }
// { dg-final { scan-lang-dump {Writing key for mergeable specialization type_decl:'::TPL@foo:1<int>'} module } }
// { dg-final { scan-lang-dump {Depset:0 specialization type_decl:'::TPL@foo:1<int>'} module } }
// { dg-final { scan-lang-dump {Voldemort:0 '::TPL@foo:1<int>'} module } }
// { dg-final { scan-lang-dump {Inserted:-1 horcrux:0@0 for '::TPL@foo:1<int>'} module } }
