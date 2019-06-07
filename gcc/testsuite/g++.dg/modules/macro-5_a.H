// { dg-additional-options "-fmodule-header -Dfoo=bar -Dbaz=1 -fdump-lang-module-vops" }
// { dg-module-bmi {} }

// { dg-final { scan-lang-dump {Writing macro #define foo at [0-9]*} module } }
// { dg-final { scan-lang-dump {Writing macro #define baz at [0-9]*} module } }
