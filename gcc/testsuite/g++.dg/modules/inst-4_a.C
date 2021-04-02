// { dg-module-do run }
// { dg-additional-options {-fmodules-ts -fdump-lang-module-graph} }

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

// { dg-final { scan-lang-dump {Pending specialization '::TPL<int>' entity:. section:. keyed to '::TPL'} module } }
// { dg-final { scan-lang-dump {Pending specialization '::TPL<int>::TPL<int>' entity:. section:. also keyed to '::TPL'} module } }
