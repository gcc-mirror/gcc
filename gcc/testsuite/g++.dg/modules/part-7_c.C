// { dg-additional-options {-fmodules-ts -fdump-lang-module-blocks} }

export module foo;
// { dg-module-cmi foo }
import :bob;
export import :bill;

int foo (frob *p)
{
  return p->field;
}

int foo (FROB<2> *p)
{
  return p->val;
}

// { dg-final { scan-lang-dump {Cluster members:\n  \[0\]=decl definition '::frob@foo:bob:1'} module } }
// { dg-final { scan-lang-dump {Cluster members:\n  \[0\]=decl definition '::template FROB@foo:bob:1'} module } }
