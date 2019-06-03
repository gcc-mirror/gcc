// { dg-additional-options "-fmodules-ts -fdump-lang-module-graph-blocks" }
// injection followed by declaration

export module foo;
// { dg-module-bmi foo }

template <typename T> class TPL
{
  friend void foo (T, void *); // { dg-warning "non-template function" }

  T member;
};

template class TPL<int>;  // instantiate

void foo (int, void *);

// { dg-final { scan-lang-dump-not {Connecting declaration decl template_decl:'::foo@foo:1'} module } }

// { dg-final { scan-lang-dump {Template friend '::foo@foo:1<int>' discovered} module } }
// { dg-final { scan-lang-dump {Cluster:1 2 depsets\n  \[0\]=decl definition '::TPL@foo:1'\n  \[1\]=binding '::TPL'} module } }
// { dg-final { scan-lang-dump {Cluster:2 2 depsets\n  \[0\]=specialization declaration '::foo@foo:1<int>'\n  \[1\]=binding '::foo'} module } }
// { dg-final { scan-lang-dump {Cluster:3 . depsets\n  \[0\]=specialization definition '::TPL@foo:1<int>'} module } }
