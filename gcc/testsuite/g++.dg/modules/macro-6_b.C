// { dg-additional-options "-fdump-lang-module" }
// { dg-module-bmi {macro} }

export module macro;
import "macro-6_a.H";

#ifndef foo
#error bad
#endif

// { dg-final { scan-lang-dump {Reading macro table "macro-6_a.H"} module } }
