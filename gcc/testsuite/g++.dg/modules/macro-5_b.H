// { dg-additional-options "-fmodule-header -Dfoo=bar -Dbaz=2 -fdump-lang-module-vops" }
// { dg-module-cmi {} }

#undef baz // not exported
#define baz 3 // exported


// { dg-final { scan-lang-dump {Writing macro #define baz at [0-9]*} module } }
