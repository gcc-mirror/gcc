// { dg-do preprocess }
// { dg-additional-options -fmodule-header }

#ifndef _STDIO_H
#define _STDIO_H
#ifdef __cplusplus
extern "C" {
#endif

  // Yes, inside extern "C" block :(
  #include "inc-xlate-1_a.H"
  #ifndef _STDARG_H
  #error barf
  #endif

#ifdef __cplusplus
}
#endif
#endif

// { dg-final { scan-file inc-xlate-1_b.i {import  "[^\n]*inc-xlate-1_a.H" \[\[__translated\]\];\n} } }
