/* Configuration for GCC for ARM running NetBSD as host.  */

#include <arm/xm-arm.h>

/* xm-netbsd.h defines this */
#ifdef HAVE_VPRINTF
#undef HAVE_VPRINTF
#endif

#ifndef SYS_SIGLIST_DECLARED
#define SYS_SIGLIST_DECLARED
#endif

#ifndef HAVE_STRERROR
#define HAVE_STRERROR
#endif

#include <xm-netbsd.h>
