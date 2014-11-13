/* Check that conversion functions link correctly with -ffast-math.  */

/* { dg-do link } */
/* { dg-options "-ffast-math -lm" }  */
/* { dg-add-options c99_runtime } */
/* Bionic doesn't have rintl */
/* { dg-require-effective-target non_bionic } */

#include "builtins-config.h"

double floor (double);
float floorf (float);
long double floorl (long double);

double ceil (double);
float ceilf (float);
long double ceill (long double);

double round (double);
float roundf (float);
long double roundl (long double);

double rint (double);
float rintf (float);
long double rintl (long double);

int ifloor (double a) { return (int) floor (a); }
#ifdef HAVE_C99_RUNTIME
int ifloorf (float a) { return (int) floorf (a); }
int ifloorl (long double a) { return (int) floorl (a); }
#endif

long lfloor (double a) { return (long) floor (a); }
#ifdef HAVE_C99_RUNTIME
long lfloorf (float a) { return (long) floorf (a); }
long lfloorl (long double a) { return (long) floorl (a); }
#endif

long long llfloor (double a) { return (long long) floor (a); }
#ifdef HAVE_C99_RUNTIME
long long llfloorf (float a) { return (long long) floorf (a); }
long long llfloorl (long double a) { return (long long) floorl (a); }
#endif

int iceil (double a) { return (int) ceil (a); }
#ifdef HAVE_C99_RUNTIME
int iceilf (float a) { return (int) ceilf (a); }
int iceill (long double a) { return (int) ceill (a); }
#endif

long lceil (double a) { return (long) ceil (a); }
#ifdef HAVE_C99_RUNTIME
long lceilf (float a) { return (long) ceilf (a); }
long lceill (long double a) { return (long) ceill (a); }
#endif

long long llceil (double a) { return (long long) ceil (a); }
#ifdef HAVE_C99_RUNTIME
long long llceilf (float a) { return (long long) ceilf (a); }
long long llceill (long double a) { return (long long) ceill (a); }
#endif

#ifdef HAVE_C99_RUNTIME
int iround (double a) { return (int) round (a); }
int iroundf (float a) { return (int) roundf (a); }
int iroundl (long double a) { return (int) roundl (a); }
#endif

#ifdef HAVE_C99_RUNTIME
int irint (double a) { return (int) rint (a); }
int irintf (float a) { return (int) rintf (a); }
int irintl (long double a) { return (int) rintl (a); }
#endif

int main () { return 0; }
