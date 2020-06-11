/****************************************************************************
 *                                                                          *
 *                         GNAT COMPILER COMPONENTS                         *
 *                                                                          *
 *                                   C A L                                  *
 *                                                                          *
 *                          C Implementation File                           *
 *                                                                          *
 *          Copyright (C) 1992-2020, Free Software Foundation, Inc.         *
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

/*  This file contains routines marked with pragmas Import in package       */
/*  GNAT.Calendar. It is used to do Duration to timeval conversion.         */
/*  These are simple wrapper functions to abstract the fact that the C      */
/*  struct timeval fields are not normalized (they are generally            */
/*  defined as int or long values).                                         */

#if defined (__vxworks)
#ifdef __RTP__
#include <time.h>
#include <version.h>
#if (_WRS_VXWORKS_MAJOR == 7) || (_WRS_VXWORKS_MINOR != 0)
#include <sys/time.h>
#endif
#else
#include <sys/times.h>
#endif
#elif defined (__nucleus__)
#include <time.h>
#else
#include <sys/time.h>
#endif

#ifdef __MINGW32__
#include "mingw32.h"
#if STD_MINGW
#include <winsock.h>
#endif
#endif

void
__gnat_timeval_to_duration (struct timeval *t, long long *sec, long *usec)
{
  *sec  = (long long) t->tv_sec;
  *usec = (long) t->tv_usec;
}

void
__gnat_duration_to_timeval (long long sec, long usec, struct timeval *t)
{
  /* here we are doing implicit conversion to the struct timeval
     fields types. */

  t->tv_sec = sec;
  t->tv_usec = usec;
}
