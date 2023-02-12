/* wraptime.c provides access to time functions.

Copyright (C) 2009-2021 Free Software Foundation, Inc.
Contributed by Gaius Mulley <gaius@glam.ac.uk>.

This file is part of GNU Modula-2.

GNU Modula-2 is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GNU Modula-2 is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  */

#include "gm2-libs-host.h"

#if defined(HAVE_SYS_TYPES_H)
#   include <sys/types.h>
#endif

#if defined(HAVE_SYS_TIME_H)
#   include <sys/time.h>
#endif

#if defined(HAVE_TIME_H)
#   include <time.h>
#endif

#if defined(HAVE_MALLOC_H)
#  include <malloc.h>
#endif

#if !defined(TRUE)
#  define TRUE  (1==1)
#endif
#if !defined(FALSE)
#  define FALSE (1==0)
#endif

/* InitTimeval returns a newly created opaque type.  */

struct timeval *
wraptime_InitTimeval (void)
{
#if defined(HAVE_TIMEVAL)
  return (struct timeval *) malloc (sizeof (struct timeval));
#else
  return NULL;
#endif
}

/* KillTimeval deallocates the memory associated with an
   opaque type.  */

struct timeval *
wraptime_KillTimeval (void *tv)
{
  free (tv);
  return NULL;
}

/* InitTimezone returns a newly created opaque type.  */

struct timezone *
wraptime_InitTimezone (void)
{
  return (struct timezone *) malloc (sizeof (struct timezone));
}

/* KillTimezone deallocates the memory associated with an
   opaque type.  */

struct timezone *
wraptime_KillTimezone (struct timezone *tv)
{
  free (tv);
  return NULL;
}

/* InitTM returns a newly created opaque type.  */

struct tm *
wraptime_InitTM (void)
{
  return (struct tm *) malloc (sizeof (struct tm));
}

/* KillTM deallocates the memory associated with an opaque type.  */

struct tm *
wraptime_KillTM (struct tm *tv)
{
  free (tv);
  return NULL;
}

/* gettimeofday calls gettimeofday(2) with the same parameters, tv,
   and, tz.  It returns 0 on success.  */

int
wraptime_gettimeofday (void *tv, struct timezone *tz)
{
  return gettimeofday (tv, tz);
}

/* settimeofday calls settimeofday(2) with the same parameters, tv,
   and, tz.  It returns 0 on success.  */

int
wraptime_settimeofday (void *tv, struct timezone *tz)
{
  return settimeofday (tv, tz);
}

/* wraptime_GetFractions returns the tv_usec field inside the timeval
   structure.  */

#if defined(HAVE_TIMEVAL)
unsigned int
wraptime_GetFractions (struct timeval *tv)
{
  return (unsigned int) tv->tv_usec;
}
#else
unsigned int
wraptime_GetFractions (void *tv)
{
  return 0;
}
#endif

/* localtime_r returns the tm parameter, m, after it has been assigned
   with appropriate contents determined by, tv.  Notice that this
   procedure function expects, timeval, as its first parameter and not
   a time_t (as expected by the posix equivalent).  */

#if defined(HAVE_TIMEVAL)
struct tm *
wraptime_localtime_r (struct timeval *tv, struct tm *m)
{
  return localtime_r (&tv->tv_sec, m);
}
#else
struct tm *
wraptime_localtime_r (void *tv, struct tm *m)
{
  return m;
}
#endif

/* wraptime_GetYear returns the year from the structure, m.  */

unsigned int
wraptime_GetYear (struct tm *m)
{
  return m->tm_year;
}

/* wraptime_GetMonth returns the month from the structure, m.  */

unsigned int
wraptime_GetMonth (struct tm *m)
{
  return m->tm_mon;
}

/* wraptime_GetDay returns the day of the month from the structure, m.  */

unsigned int
wraptime_GetDay (struct tm *m)
{
  return m->tm_mday;
}

/* wraptime_GetHour returns the hour of the day from the structure, m.  */

unsigned int
wraptime_GetHour (struct tm *m)
{
  return m->tm_hour;
}

/* wraptime_GetMinute returns the minute within the hour from the structure, m.  */

unsigned int
wraptime_GetMinute (struct tm *m)
{
  return m->tm_min;
}

/* wraptime_GetSecond returns the seconds in the minute from the
   structure, m.  The return value will always be in the range 0..59.
   A leap minute of value 60 will be truncated to 59.  */

unsigned int
wraptime_GetSecond (struct tm *m)
{
  if (m->tm_sec == 60)
    return 59;
  else
    return m->tm_sec;
}

/* wraptime_GetSummerTime returns true if summer time is in effect.  */

unsigned int
wraptime_GetSummerTime (struct timezone *tz)
{
  return tz->tz_dsttime != 0;
}

/* wraptime_GetDST returns the number of minutes west of GMT.  */

int
wraptime_GetDST (struct timezone *tz)
{
  return tz->tz_minuteswest;
}

/* SetTimezone set the timezone field inside timeval, tv.  */

void
wraptime_SetTimezone (struct timezone *tz,
		      int zone, int minuteswest)
{
  tz->tz_dsttime = zone;
  tz->tz_minuteswest = minuteswest;
}

/* SetTimeval sets the fields in tm, t, with:
   second, minute, hour, day, month, year, fractions.  */

#if defined(HAVE_TIMEVAL)
void
wraptime_SetTimeval (struct tm *t,
		     unsigned int second,
		     unsigned int minute,
		     unsigned int hour,
		     unsigned int day,
		     unsigned int month,
		     unsigned int year,
		     unsigned int yday,
		     unsigned int wday,
		     unsigned int isdst)
{
  t->tm_sec = second;
  t->tm_min = minute;
  t->tm_hour = hour;
  t->tm_mday = day;
  t->tm_mon = month;
  t->tm_year = year;
  t->tm_yday = yday;
  t->tm_wday = wday;
  t->tm_isdst = isdst;
}
#else
wraptime_SetTimeval (void *t,
		     unsigned int second,
		     unsigned int minute,
		     unsigned int hour,
		     unsigned int day,
		     unsigned int month,
		     unsigned int year,
		     unsigned int yday,
		     unsigned int wday,
		     unsigned int isdst)
{
  return t;
}
#endif

/* init/finish functions for the module.  */

void
_M2_wraptime_init ()
{}

void
_M2_wraptime_finish ()
{}
