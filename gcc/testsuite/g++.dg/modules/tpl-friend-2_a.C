// { dg-additional-options "-fmodules-ts -fdump-lang-module-graph-blocks" }
// injection followed by declaration

export module foo;
// { dg-module-cmi foo }

template <typename T> class TPL
{
  friend void foo (T, void *); // { dg-warning "non-template function" }

  T member;
};

template class TPL<int>;  // instantiate

void foo (int, void *);

// { dg-final { scan-lang-dump-not {Connecting declaration decl template_decl:'::foo'} module } }

// { dg-final { scan-lang-dump {Cluster members:\n  \[0\]=decl definition '::TPL'\n  \[1\]=binding '::TPL'} module } }
// { dg-final { scan-lang-dump {Cluster members:\n  \[0\]=specialization declaration '::foo<int>'\n  \[1\]=binding '::foo'} module } }
// { dg-final { scan-lang-dump {Cluster members:\n  \[0\]=specialization definition '::TPL<int>'} module } }
