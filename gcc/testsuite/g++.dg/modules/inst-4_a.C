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

// { dg-final { scan-lang-dump {Specialization '::TPL<int>' entity:. keyed to foo\[.\] '::template TPL'} module } }
// { dg-final { scan-lang-dump {Specialization '::TPL<int>::TPL<int>' entity:. keyed to foo\[.\] '::template TPL<T>::template TPL'} module } }
