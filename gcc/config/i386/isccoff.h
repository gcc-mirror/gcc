/* Definitions for Intel 386 running Interactive Unix System V.
   Specifically, this is for recent versions that support POSIX;
   for version 2.0.2, use configuration option i386-sysv instead.
   (But set TARGET_DEFAULT to (MASK_80307 | MASK_FLOAT_RETURNS)
   if you do that, if you don't have a real 80387.)  */

/* Mostly it's like AT&T Unix System V. */

#include "i386/sysv3.h"

/* But with a few changes.  */
#include "i386/isc.h"
