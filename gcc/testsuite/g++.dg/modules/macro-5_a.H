// { dg-additional-options "-fmodule-header -Dfoo=bar -Dbaz=1 -fdump-lang-module-vops" }
// command line macros are not exported
// { dg-module-cmi {} }

// { dg-final { scan-lang-dump-not {Writing macro #define [_a-zA-Z0-9]* at [0-9]*} module } }
