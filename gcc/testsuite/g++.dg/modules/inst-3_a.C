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

// { dg-final { scan-lang-dump {Cluster members:\n(  \[.\]=[^\n]*'\n)*  \[.\]=specialization definition '::TPL<int>'\n  \[.\]=specialization declaration '::TPL<int>::TPL<int>'\n} module } }
// { dg-final { scan-lang-dump {Writing:-[0-9]*'s type spec merge key \(specialization\) type_decl:'::TPL<int>'} module } }
// { dg-final { scan-lang-dump {Depset:. specialization entity:. type_decl:'::TPL<int>'} module } }
