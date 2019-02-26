// { dg-additional-options -fmodule-header=<stdarg.h> }
// { dg-module-bmi <stdarg.h> }

#ifndef _STDARG_H
#define _STDARG_H
#ifdef __cplusplus
extern "C" {
#endif

  extern void frob ();

#ifdef __cplusplus
}
#endif
#endif
