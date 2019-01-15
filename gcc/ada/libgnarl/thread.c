/****************************************************************************
 *                                                                          *
 *                         GNAT COMPILER COMPONENTS                         *
 *                                                                          *
 *                              P T H R E A D                               *
 *                                                                          *
 *                          C Implementation File                           *
 *                                                                          *
 *          Copyright (C) 2011-2019, Free Software Foundation, Inc.         *
 *                                                                          *
 * GNAT is free software;  you can  redistribute it  and/or modify it under *
 * terms of the  GNU General Public License as published  by the Free Soft- *
 * ware  Foundation;  either version 3,  or (at your option) any later ver- *
 * sion.  GNAT is distributed in the hope that it will be useful, but WITH- *
 * OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY *
 * or FITNESS FOR A PARTICULAR PURPOSE.                                     *
 *                                                                          *
 * As a special exception under Section 7 of GPL version 3, you are granted *
 * additional permissions described in the GCC Runtime Library Exception,   *
 * version 3.1, as published by the Free Software Foundation.               *
 *                                                                          *
 * You should have received a copy of the GNU General Public License and    *
 * a copy of the GCC Runtime Library Exception along with this program;     *
 * see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    *
 * <http://www.gnu.org/licenses/>.                                          *
 *                                                                          *
 * GNAT was originally developed  by the GNAT team at  New York University. *
 * Extensive contributions were provided by Ada Core Technologies Inc.      *
 *                                                                          *
 ****************************************************************************/

/*  This file provides utility functions to access the threads API          */

#include "s-oscons.h"

/* If the clock we used for tasking (CLOCK_RT_Ada) is not the default
 * CLOCK_REALTIME, we need to set cond var attributes accordingly.
 */
#if CLOCK_RT_Ada != CLOCK_REALTIME
# include <pthread.h>
# include <time.h>

int
__gnat_pthread_condattr_setup(pthread_condattr_t *attr) {
  return pthread_condattr_setclock (attr, CLOCK_RT_Ada);
}

#else

int
__gnat_pthread_condattr_setup (void *attr) {
  /* Dummy version for other platforms, which may or may not have pthread.h */
  return 0;
}

#endif

#if defined (__APPLE__)
#include <mach/mach.h>
#include <mach/clock.h>
#endif

/* Return the clock ticks per nanosecond for Posix systems lacking the
   Posix extension function clock_getres, or else 0 nsecs on error.  */

int
__gnat_clock_get_res (void)
{
#if defined (__APPLE__)
  clock_serv_t clock_port;
  mach_msg_type_number_t count;
  int nsecs;
  int result;

  count = 1;
  result = host_get_clock_service
    (mach_host_self (), SYSTEM_CLOCK, &clock_port);

  if (result == KERN_SUCCESS)
    result = clock_get_attributes (clock_port, CLOCK_GET_TIME_RES,
      (clock_attr_t) &nsecs, &count);

  if (result == KERN_SUCCESS)
    return nsecs;
#endif

  return 0;
}
