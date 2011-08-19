/* Check that conversion functions don't leak into global namespace.  */

/* { dg-do link } */
/* { dg-options "-ffast-math" }  */
/* { dg-add-options c99_runtime } */

#include "../../gcc.dg/builtins-config.h"

int ifloor (double a) { return __builtin_ifloor (a); }
#ifdef HAVE_C99_RUNTIME
int ifloorf (float a) { return __builtin_ifloorf (a); }
int ifloorl (long double a) { return __builtin_ifloorl (a); }
#endif

long lfloor (double a) { return __builtin_lfloor (a); }
#ifdef HAVE_C99_RUNTIME
long lfloorf (float a) { return __builtin_lfloorf (a); }
long lfloorl (long double a) { return __builtin_lfloorl (a); }
#endif

long long llfloor (double a) { return __builtin_llfloor (a); }
#ifdef HAVE_C99_RUNTIME
long long llfloorf (float a) { return __builtin_llfloorf (a); }
long long llfloorl (long double a) { return __builtin_llfloorl (a); }
#endif

int iceil (double a) { return __builtin_iceil (a); }
#ifdef HAVE_C99_RUNTIME
int iceilf (float a) { return __builtin_iceilf (a); }
int iceill (long double a) { return __builtin_iceill (a); }
#endif

long lceil (double a) { return __builtin_lceil (a); }
#ifdef HAVE_C99_RUNTIME
long lceilf (float a) { return __builtin_lceilf (a); }
long lceill (long double a) { return __builtin_lceill (a); }
#endif

long long llceil (double a) { return __builtin_llceil (a); }
#ifdef HAVE_C99_RUNTIME
long long llceilf (float a) { return __builtin_llceilf (a); }
long long llceill (long double a) { return __builtin_llceill (a); }
#endif

int iround (double a) { return __builtin_iround (a); }
#ifdef HAVE_C99_RUNTIME
int iroundf (float a) { return __builtin_iroundf (a); }
int iroundl (long double a) { return __builtin_iroundl (a); }
#endif

int irint (double a) { return __builtin_irint (a); }
#ifdef HAVE_C99_RUNTIME
int irintf (float a) { return __builtin_irintf (a); }
int irintl (long double a) { return __builtin_irintl (a); }
#endif

int main () { return 0; }
