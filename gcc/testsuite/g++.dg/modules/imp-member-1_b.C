// { dg-additional-options "-fmodules-ts -fdump-lang-module-blocks" }
export module B;
// { dg-module-cmi B }
export import A;

export struct D
{
  C c;

  // ctor causes C::C to exist, and we need to put it in out CMI
  inline D (){}
};

// { dg-final { scan-lang-dump {\[.*\]=decl definition '::C@A:1::__ct '} module } }
