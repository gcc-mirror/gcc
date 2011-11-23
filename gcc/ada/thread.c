/****************************************************************************
 *                                                                          *
 *                         GNAT COMPILER COMPONENTS                         *
 *                                                                          *
 *                              P T H R E A D                               *
 *                                                                          *
 *                          C Implementation File                           *
 *                                                                          *
 *             Copyright (C) 2011, Free Software Foundation, Inc.           *
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

#ifdef NEED_PTHREAD_CONDATTR_SETCLOCK
# include <pthread.h>
# include <time.h>

int
__gnat_pthread_condattr_setup(pthread_condattr_t *attr) {
/*
 * If using a clock other than CLOCK_REALTIME for the Ada Monotonic_Clock,
 * the corresponding clock id must be set for condition variables.
 */
  return pthread_condattr_setclock (attr, CLOCK_RT_Ada);
}

#else

int
__gnat_pthread_condattr_setup (void *attr) {
  /* Dummy version for other platforms, which may or may not have pthread.h */
  return 0;
}

#endif
