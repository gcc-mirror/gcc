// { dg-additional-options "-fmodules-ts -fdump-lang-module-blocks" }
export module A;
// { dg-module-cmi A }

struct M
{
  M (){}
};

export struct C 
{
  M m;
  // lazy implicit ctor
};

// { dg-final { scan-lang-dump-not {'::C::__ct '} module } }
