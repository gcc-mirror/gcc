/* PR libstdc++/85466 */
/* { dg-do run } */
/* { dg-require-effective-target c99_runtime } */
/* { dg-options "-O2 -fno-builtin" } */
/* { dg-add-options ieee } */

#include <stdlib.h>

#if defined(__GLIBC__) && defined(__GLIBC_PREREQ)
# if !__GLIBC_PREREQ (2, 24)
/* Workaround buggy nextafterl in glibc 2.23 and earlier,
   see https://sourceware.org/bugzilla/show_bug.cgi?id=20205  */
#  define NO_LONG_DOUBLE 1
# endif
#endif

#if defined(_ARCH_PPC) && defined(__LONG_DOUBLE_IEEE128__)
/* On PowerPC systems, long double uses either the IBM long double format, or
   IEEE 128-bit format.  The compiler switches the long double built-in
   function names and glibc switches the names when math.h is included.
   Because this test is run with -fno-builtin, include math.h so that the
   appropriate nextafter functions are called.  */
#include <math.h>
#endif

#include "nextafter-1.c"
