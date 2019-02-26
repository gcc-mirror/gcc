// { dg-do preprocess }
// { dg-additional-options "-fmodule-header=<stdio.h> -fmodule-mapper=|cxx-mapper\\ -f\\ $srcdir/g++.dg/modules/inc-xlate-1.map" }

#ifndef _STDIO_H
#define _STDIO_H
#ifdef __cplusplus
extern "C" {
#endif

  // Yes, inside extern "C" block :(
  #include <stdarg.h>
  #ifndef _STDARG_H
  #error barf
  #endif

#ifdef __cplusplus
}
#endif
#endif

// { dg-final { scan-file inc-xlate-1_b.i "__import <stdarg.h>;\n" } }
