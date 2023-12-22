/* syslimits.h stands for the system's own limits.h file.
   If we can use it ok unmodified, then we install this text.
   If fixincludes fixes it, then the fixed version is installed
   instead of this text.  */

#define _GCC_NEXT_LIMITS_H		/* tell gcc's limits.h to recurse */
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wpedantic" // include_next
#include_next <limits.h>
#pragma GCC diagnostic pop
#undef _GCC_NEXT_LIMITS_H
