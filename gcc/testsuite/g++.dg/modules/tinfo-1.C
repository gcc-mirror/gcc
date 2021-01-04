// { dg-additional-options "-fmodules-ts -fdump-lang-module-uid" }
export module Foo;
// { dg-module-cmi Foo }

struct B 
{
};

struct C : B 
{
};

inline void foo ()
{
  throw C ();
}

// { dg-final { scan-lang-dump {Writing typedef type_decl:'::__si_class_type_info_pseudo_9'} module } }
// { dg-final { scan-lang-dump {Wrote tinfo_type:-[0-9]* 9 '::__si_class_type_info_pseudo_9'} module } }
// { dg-final { scan-lang-dump-not {Writing tree:-[0-9]* type_decl:'__si_class_type_info_pseudo_9'} module } }
