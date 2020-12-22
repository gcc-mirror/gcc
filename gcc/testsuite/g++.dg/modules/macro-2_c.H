// { dg-additional-options "-fmodule-header -fdump-lang-module" }
// { dg-module-cmi {} }

#ifndef MACRO_2c_H
#define MACRO_2c_H
import "macro-2_a.H";

#endif

// { dg-final { scan-lang-dump-not {Read new macro #define MACRO_2a_H at} module } }
