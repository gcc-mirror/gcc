// { dg-module-do run }
// { dg-additional-options {-fmodule-header -fdump-lang-module-blocks} }
// { dg-module-cmi {} }

static int bob (int x)
{
  return x;
}

// { dg-final { scan-lang-dump { Cluster members:\n  \[0\]=decl definition '::bob'\n  \[1\]=binding '::bob'} module } }
