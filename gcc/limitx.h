/* This administrivia gets added to the beginning of limits.h
   if the system has its own version of limits.h.  */

#ifndef _LIMITS_H_  /* Terminated in limity.h.  */
#define _LIMITS_H_

#ifndef _LIBC_LIMITS_H_
/* Use <...> so that we find syslimits.h only in system include dirs.  */
#include <syslimits.h>
#endif
