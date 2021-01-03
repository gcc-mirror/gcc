/* PR libstdc++/85466 */
/* { dg-do run } */
/* { dg-require-effective-target c99_runtime } */
/* { dg-options "-O2 -fno-builtin" } */
/* { dg-add-options ieee } */

#include <stdlib.h>

/* In order to run on systems like the PowerPC that have 3 different long
   double types, include math.h so it can choose what is the appropriate
   nextafterl function to use.

   If we didn't use -fno-builtin for this test, the PowerPC compiler would have
   changed the names of the built-in functions that use long double.  The
   nextafter-1.c function runs with this mapping.

   Since this test uses -fno-builtin, include math.h, so that math.h can make
   the appropriate choice to use.  */
#include <math.h>

#if defined(__GLIBC__) && defined(__GLIBC_PREREQ)
# if !__GLIBC_PREREQ (2, 24)
/* Workaround buggy nextafterl in glibc 2.23 and earlier,
   see https://sourceware.org/bugzilla/show_bug.cgi?id=20205  */
#  define NO_LONG_DOUBLE 1
# endif
#endif

#define _NEXT_AFTER_2

#include "nextafter-1.c"
