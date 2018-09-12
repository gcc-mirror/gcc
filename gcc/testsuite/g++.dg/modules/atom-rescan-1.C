// { dg-options "-fmodules-atom -fdump-lang-module" }
// { dg-module-bmi bob }

export module bob;

#if 1 // { dg-message "module preamble ended immediately before" }
// { dg-message "explicitly mark" "" { target *-*-* } .-1 }
int i;
#endif
int j;

// { dg-final { scan-lang-dump {Preamble ends after 1} module } }
