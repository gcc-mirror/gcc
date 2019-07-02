// { dg-additional-options "-fmodules-ts -fdump-lang-module" }

export module macro;
// { dg-module-cmi {macro} }
import "macro-6_a.H";

#ifndef foo
#error bad
#endif

// { dg-final { scan-lang-dump {Reading macro table [^\n]*macro-6_a.H} module } }
