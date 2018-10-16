// { dg-additional-options "-fmodule-legacy=macro-5_a.H -Dfoo=bar -Dbaz=1 -fdump-lang-module" }
// { dg-module-bmi {"macro-5_a.H"} }

// { dg-final { scan-lang-dump {Writing macro #define foo at [0-9]*} module } }
// { dg-final { scan-lang-dump {Writing macro #define baz at [0-9]*} module } }
