// { dg-additional-options "-fmodule-legacy=macro-2_c.H -fdump-lang-module" }
// { dg-module-bmi {"macro-2_c.H"} }

#ifndef MACRO_2c_H
#define MACRO_2c_H
import "macro-2_a.H";

#endif

// { dg-final { scan-lang-dump {Controlling macro is MACRO_2a_H} module } }
// { dg-final { scan-lang-dump-not {Read new macro #define MACRO_2a_H at} module } }
// { dg-final { scan-lang-dump {Selecting MACRO_2c_H as controlling macro} module } }
