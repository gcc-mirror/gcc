/* Configuration for GCC for ns32k running NetBSD as host.  */

#include <ns32k/xm-ns32k.h>

/* ns32k/xm-ns32k.h defines these macros, but we don't need them */
#undef memcmp
#undef memcpy
#undef memset

#include <xm-netbsd.h>
