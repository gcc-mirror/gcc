/* Copyright (C) 2003 Free Software Foundation.

   Define macros useful in tests for bulitin functions.  */

/* Define HAVE_C99_RUNTIME if the entire C99 runtime is available on
   the target system.  The value of HAVE_C99_RUNTIME should be the
   same as the value of TARGET_C99_FUNCTIONS in the GCC machine
   description.  (Perhaps GCC should predefine a special macro
   indicating whether or not TARGET_C99_FUNCTIONS is set, but it does
   not presently do that.)  */

#if defined(__hppa) && defined(__hpux)
/* PA HP-UX doesn't have the entire C99 runtime.  */
#elif defined(__sun)
/* Solaris doesn't have the entire C99 runtime.  */
#elif defined(__sgi)
/* Irix6 doesn't have the entire C99 runtime.  */
#elif defined(__FreeBSD__) && (__FreeBSD__ < 5)
/* FreeBSD before version 5 doesn't have the entire C99 runtime. */
#elif defined(__netware__)
/* NetWare doesn't have the entire C99 runtime.  */
#elif (defined(__APPLE__) \
       && ! defined (__ENVIRONMENT_MAC_OS_X_VERSION_MIN_REQUIRED__))
/* MacOS versions before 10.3 don't have many C99 functions.  
   But, if you're including this file, you probably want to test the
   newer behaviour, so: */
#error forgot to set -mmacosx-version-min.
#elif (defined(__APPLE__) \
       && __ENVIRONMENT_MAC_OS_X_VERSION_MIN_REQUIRED__ < 1030)
/* MacOS versions before 10.3 don't have many C99 functions.  */
#else
/* Newlib has the "f" variants of the math functions, but not the "l"
   variants.  TARGET_C99_FUNCTIONS is only defined if all C99
   functions are present.  Therefore, on systems using newlib, tests
   of builtins will fail the "l" variants, and we should therefore not
   define HAVE_C99_RUNTIME.  Including <sys/types.h> gives us a way of
   seeing if _NEWLIB_VERSION is defined.  Including <math.h> would work
   too, but the GLIBC math inlines cause us to generate inferior code,
   which causes the test to fail, so it is not safe.  Including <limits.h>
   also fails because the include search paths are ordered such that GCC's
   version will be found before the newlib version.  */
#include <sys/types.h>
#ifdef _NEWLIB_VERSION
#else
#define HAVE_C99_RUNTIME
#endif
#endif
