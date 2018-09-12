// { dg-options "-fmodules-atom -fdump-lang-module" }
// { dg-module-bmi bob }

export module bob;
; // explict end of preamble marker
#if 1

int i;
#endif
int j;

// { dg-final { scan-lang-dump-not {Preamble ends after} module } }
