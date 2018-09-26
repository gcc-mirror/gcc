// { dg-additional-options "-fmodule-legacy=macro-2_c.H -fmodules-atom -fdump-lang-module" }
// { dg-module-bmi {"macro-2_c.H"} }

#ifndef MACRO_2c_H
#define MACRO_2c_H
import "macro-2_a.H";

#endif

// { dg-final { scan-lang-dump-not { Writing macro #define MACRO_2a_H at} module } }
// { dg-final { scan-lang-dump { Read new macro #define MACRO_2a_H at} module } }
// { dg-final { scan-lang-dump { Writing macro #define MACRO_2c_H at} module } }
